extern crate petgraph;
extern crate wikipedia;

use petgraph::dot::Dot;
use rand::prelude::SliceRandom;

use rayon::prelude::*;

use std::fmt::*;
use std::fs;

use log::LevelFilter;
use log::*;
use log4rs::append::console::ConsoleAppender;
use log4rs::append::file::FileAppender;
use log4rs::config::{Appender, Config, Logger, Root};

const BREADTH: i32 = 5;
const DEPTH: i32 = 4;

const DEBUG_TARGET: &str = "debug";
const GRAPH_TARGET: &str = "graph";

fn setup_logger() {
    let stdout = ConsoleAppender::builder()
        .target(log4rs::append::console::Target::Stdout)
        .build();

    let graph = FileAppender::builder()
        .encoder(Box::new(log4rs::encode::pattern::PatternEncoder::new(
            "{m}",
        )))
        .build("graph.dot")
        .unwrap();

    let stderr = ConsoleAppender::builder()
        .target(log4rs::append::console::Target::Stderr)
        .build();

    log4rs::init_config(
        Config::builder()
            .appender(Appender::builder().build("graph", Box::new(graph)))
            .appender(Appender::builder().build("stdout", Box::new(stdout)))
            .appender(Appender::builder().build("stderr", Box::new(stderr)))
            .logger(
                Logger::builder()
                    .appender("graph")
                    .additive(false)
                    .build(GRAPH_TARGET, LevelFilter::Info),
            )
            .logger(
                Logger::builder()
                    .appender("stderr")
                    .additive(false)
                    .build(DEBUG_TARGET, LevelFilter::Info),
            )
            .build(Root::builder().appender("stdout").build(LevelFilter::Info))
            .unwrap(),
    )
    .unwrap();
}

fn timed<T>(label: &str, f: &mut FnMut() -> T) -> T {
    let now = std::time::Instant::now();
    let result = f();
    info!("{} took {}", label, now.elapsed().as_micros());
    result
}

fn main() {
    setup_logger();

    let mut graph: petgraph::graph::UnGraph<String, String> =
        petgraph::graph::UnGraph::new_undirected();

    let mut words: Vec<String> = fs::read_to_string("/usr/share/dict/words")
        .expect("Error reading dictionary")
        .lines()
        .map(|line| line.to_owned())
        .collect();
    words.shuffle(&mut rand::thread_rng());
    let search_terms = words[0..2].to_vec();

    let wiki = wikipedia::Wikipedia::<wikipedia::http::default::Client>::default();
    let wiki_ref = &wiki;

    let results = timed("search_wikis", &mut || search_wiki(wiki_ref, &search_terms));

    let terms = results.iter().map(|n| &n.1).collect::<Vec<_>>();

    info!("Crawling articles: '{:?}'", terms);

    let updates = results
        .par_iter()
        .map(|term_to_title| {
            debug!(
                target: DEBUG_TARGET,
                "Spawned thread {:?}",
                std::thread::current().id()
            );
            let mut results = vec![];
            let ref_results = &mut results;
            timed(&format!("crawl title: {}", &term_to_title.1), &mut || {
                crawl_from_title(wiki_ref, &term_to_title.1, ref_results, BREADTH, 0)
            });
            debug!(
                target: DEBUG_TARGET,
                "Received {} results crawling '{}'",
                results.len(),
                term_to_title.1
            );
            results
        })
        .flatten()
        .collect::<Vec<_>>();

    debug!(target: DEBUG_TARGET, "Generated {} updates.", updates.len());

    let g_ref = &mut graph;
    let (nodes, edges) = timed("update_graph", &mut || update_graph(&updates, g_ref));
    debug!(target: DEBUG_TARGET, "{} nodes, {} edges", nodes, edges);

    let endpoints = results
        .iter()
        .map(|n| &n.1)
        .map(|n| g_ref.node_indices().find(|ni| &g_ref[*ni] == n).unwrap())
        .collect::<Vec<_>>();

    debug!(
        target: DEBUG_TARGET,
        "Generated graph with {} nodes and {} edges",
        graph.node_count(),
        graph.edge_count()
    );

    timed("astar", &mut || match petgraph::algo::astar(
        &graph,
        endpoints[0],
        |n| n == endpoints[1],
        |_| 1,
        |_| 0,
    ) {
        Some((_, path)) => info!("{:?}", path),
        None => info!("No path found between endpoints: {:?}", endpoints),
    });

    info!(target: GRAPH_TARGET, "{}", Dot::new(&graph));
}

fn crawl_from_title(
    wiki: &wikipedia::Wikipedia<wikipedia::http::default::Client>,
    title: &String,
    updates_so_far: &mut Vec<(String, String)>,
    breadth: i32,
    depth: i32,
) {
    if depth >= DEPTH {
        return;
    }
    let mut links = get_wiki_links(wiki, title);
    if links.is_empty() {
        debug!(
            target: DEBUG_TARGET,
            "Received no links from page '{}'", title
        );
        return;
    }
    debug!(
        target: DEBUG_TARGET,
        "Received {} links for page '{}'",
        links.len(),
        title
    );
    links.shuffle(&mut rand::thread_rng());

    updates_so_far.extend(
        links[..std::cmp::min(breadth as usize, links.len() - 1 as usize)]
            .par_iter()
            .map(|linked_title| {
                let mut updates = vec![(title.to_owned(), linked_title.to_owned())];
                crawl_from_title(wiki, linked_title, &mut updates, breadth - 1, depth + 1);
                updates
            })
            .flatten()
            .collect::<Vec<_>>(),
    )
}

fn update_graph(
    res: &Vec<(String, String)>,
    graph: &mut petgraph::graph::UnGraph<String, String>,
) -> (i32, i32) {
    let mut tup = (0, 0);
    let tup_ref = &mut tup;
    res.iter().for_each(|adj| {
        // let mut src_ni;
        let src_ni = if let Some(node_idx) = graph.node_indices().find(|ni| graph[*ni] == adj.0) {
            node_idx
        } else {
            tup_ref.0 += 1;
            graph.add_node(adj.0.to_owned())
        };
        let dst_ni = if let Some(node_idx) = graph.node_indices().find(|ni| graph[*ni] == adj.1) {
            node_idx
        } else {
            tup_ref.0 += 1;
            graph.add_node(adj.1.to_owned())
        };
        graph.add_edge(src_ni, dst_ni, "".to_owned());
        tup_ref.1 += 1;
    });
    return tup;
}

fn search_wiki(
    wiki: &wikipedia::Wikipedia<wikipedia::http::default::Client>,
    search_terms: &Vec<String>,
) -> Vec<(String, String)> {
    search_terms
        .par_iter()
        .map(|term| match wiki.search(term) {
            Ok(results) => {
                if results.is_empty() {
                    Err(WikiError::new(format!(
                        "No results for search term '{}'",
                        term
                    )))
                } else {
                    Ok((
                        term.to_owned(),
                        results.choose(&mut rand::thread_rng()).unwrap().to_owned(),
                    ))
                }
            }
            Err(err) => Err(WikiError::new(format!(
                "Error searching for '{}': {}",
                term, err
            ))),
        })
        .inspect(|r| match r {
            Ok(results) => debug!(
                target: DEBUG_TARGET,
                "Lookup for '{}': {}", results.0, results.1
            ),
            Err(err) => error!("Lookup failed: {}", err),
        })
        .collect::<std::result::Result<Vec<(String, String)>, WikiError>>()
        .unwrap_or(vec![])
}

fn get_wiki_links(
    wiki: &wikipedia::Wikipedia<wikipedia::http::default::Client>,
    title: &String,
) -> Vec<String> {
    wiki.page_from_title(title.to_owned())
        .get_links()
        .map_err(|err| {
            WikiError::new(format!(
                "Unable to get links for page '{}', cause: {}",
                title, err
            ))
        })
        .map(|iter| iter.map(|link| link.title).collect::<Vec<_>>())
        .unwrap_or(vec![])
}

#[derive(Debug, Clone)]
struct WikiError {
    details: String,
}

impl std::fmt::Display for WikiError {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        write!(f, "{}", self.details)
    }
}

impl WikiError {
    fn new(msg: String) -> WikiError {
        WikiError {
            details: msg.to_string(),
        }
    }
}
