extern crate petgraph;
extern crate wikipedia;

use petgraph::dot::Dot;
use rand::prelude::SliceRandom;

use rayon::prelude::*;

use std::fmt::*;
use std::fs;

const BREADTH: i32 = 1;
const DEPTH: i32 = 1;

fn main() {
    let mut graph: petgraph::graph::Graph<String, String> = petgraph::graph::Graph::new();

    let mut words: Vec<String> = fs::read_to_string("/usr/share/dict/words")
        .expect("Error reading dictionary")
        .lines()
        .map(|line| line.to_owned())
        .collect();
    words.shuffle(&mut rand::thread_rng());
    let search_terms = words[0..2].to_vec();

    let wiki = wikipedia::Wikipedia::<wikipedia::http::default::Client>::default();
    let wiki_ref = &wiki;

    let updates = search_wiki(wiki_ref, &search_terms)
        .par_iter()
        .map(|term_to_title| {
            let mut results = vec![];
            crawl_from_title(wiki_ref, &term_to_title.1, &mut results, BREADTH, 0);
            println!(
                "Received {} results crawling '{}'",
                results.len(),
                term_to_title.1
            );
            results
        })
        .flatten()
        .collect::<Vec<_>>();

    println!("Generated {} updates.", updates.len());

    let (nodes, edges) = update_graph(&updates, &mut graph);
    println!("{} nodes, {} edges", nodes, edges);

    println!(
        "Generated graph with {} nodes and {} edges",
        graph.node_count(),
        graph.edge_count()
    );

    println!("{}", Dot::new(&graph));
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
        println!("Received no links from page '{}'", title);
        return;
    }
    println!("Received {} links for page '{}'", links.len(), title);
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
    graph: &mut petgraph::graph::Graph<String, String>,
) -> (i32, i32) {
    let mut tup = (0, 0);
    let tup_ref = &mut tup;
    res.iter().for_each(|adj| {
        println!("IS IT HERE?");
        // let mut src_ni;
        let src_ni = if let Some(node_idx) = graph.node_indices().find(|ni| {
            if graph[*ni] == adj.0 {
                println!("found {}", adj.0);
                true
            } else {
                false
            }
        }) {
            println!("Returning existing node {:?}", node_idx);
            node_idx
        } else {
            tup_ref.0 += 1;
            println!("Adding {}", adj.0.to_owned());
            graph.add_node(adj.0.to_owned())
        };
        let dst_ni = if let Some(node_idx) = graph.node_indices().find(|ni| graph[*ni] == adj.1) {
            println!("Returning existing node {:?}", node_idx);
            node_idx
        } else {
            tup_ref.0 += 1;
            println!("Adding {}", adj.1.to_owned());
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
            Err(err) => Err(WikiError::new(format!("{}", term))),
        })
        .inspect(|r| match r {
            Ok(results) => println!("Lookup for '{}': {}", results.0, results.1),
            Err(err) => println!("Lookup failed: {}", err),
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
