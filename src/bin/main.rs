extern crate petgraph;
extern crate wikipedia;

use petgraph::dot::Dot;
use rand::prelude::SliceRandom;

use rayon::prelude::*;

use std::fmt::*;
use std::fs;
use std::sync::Arc;
use std::sync::Mutex;
use std::thread;

const BREADTH: u32 = 5;
const DEPTH: u32 = 3;

fn main2() {
    let g = Arc::new(Mutex::new(petgraph::graph::Graph::new()));

    let handle = thread::spawn(|| {
        let wiki = wikipedia::Wikipedia::<wikipedia::http::default::Client>::default();
        let result = wiki.search("Feldenkrais").unwrap();
        if result.len() == 0 {
            println!("No results found for search.");
            return;
        }
        let page = wiki.page_from_title(result[0].to_owned());
        iterate(
            &wiki,
            g,
            &mut rand::thread_rng(),
            &page.get_title().unwrap(),
            BREADTH,
            0,
        )
        .unwrap();
    });

    handle.join().unwrap();
    // println!("{}", Dot::new(*g.lock().unwrap()));
}

fn main() {
    let mut words: Vec<String> = fs::read_to_string("/usr/share/dict/words")
        .expect("Error reading dictionary")
        .lines()
        // This is a copy
        .map(|line| line.to_owned())
        .collect();
    words.shuffle(&mut rand::thread_rng());
    let search_terms = words[0..10].to_vec();
    println!("Searching for terms: {:?}", search_terms);

    let wiki = wikipedia::Wikipedia::<wikipedia::http::default::Client>::default();

    let res = search_terms
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
        // TODO(wyszynski): Figure a better way to deal with result here.
        .unwrap()
        .into_iter()
        .map(|tup| tup.1)
        .collect::<Vec<String>>()
        .par_iter()
        .map(|title| {
            wiki.page_from_title(title.to_owned())
                .get_links()
                .map_err(|err| {
                    WikiError::new(format!(
                        "Unable to get links for page '{}', cause: {}",
                        title, err
                    ))
                })
                .map(|iter| {
                    (
                        // TODO(wyszynski): Extra copying.
                        title.to_owned(),
                        iter.map(|link| link.title).collect::<Vec<_>>(),
                    )
                })
        })
        .collect::<std::result::Result<Vec<_>, WikiError>>();

    println!("{:?}", res.unwrap());
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

fn iterate(
    wiki: &wikipedia::Wikipedia<wikipedia::http::default::Client>,
    graph: Arc<Mutex<petgraph::graph::Graph<String, String>>>,
    rng: &mut rand::rngs::ThreadRng,
    curr_title: &std::string::String,
    breadth: u32,
    depth: u32,
) -> std::result::Result<(), WikiError> {
    let page = wiki.page_from_title(curr_title.to_owned());

    // Get neighbor links.
    let links = page.get_links().map_err(|err| {
        WikiError::new(format!(
            "Unable to get links for page '{}', cause: {}",
            curr_title, err
        ))
    })?;

    let mut next_titles = vec![];

    {
        let mut guard = graph.lock().unwrap();
        // Get neighboring links from wikipedia that we don't already know about.
        let mut titles: Vec<(String, petgraph::graph::NodeIndex)> = links
            .map(|link| {
                (
                    link.title.clone(),
                    guard
                        .node_indices()
                        .find(|n| guard[*n] == link.title.to_owned()),
                )
            })
            .filter_map(|tup| {
                if let Some(node_index) = tup.1 {
                    Some((tup.0, node_index))
                } else {
                    None
                }
            })
            .collect();

        // Shuffle the link titles.
        titles.shuffle(rng);

        let curr_node = guard
            .node_indices()
            .find(|n| guard[*n] == curr_title.to_owned())
            .unwrap();

        // Add a new node for each link and recurse.
        next_titles = titles[0..std::cmp::min(titles.len(), breadth as usize)]
            .iter()
            .map(|tup| {
                let neighbor_node = guard.add_node(tup.0.to_owned());
                guard.add_edge(curr_node, neighbor_node, "".to_string());
                tup.0.clone()
            })
            .collect();
    }

    // let v: Vec<Result<(), std::fmt::Error>> = next_titles
    //     .par_iter()
    //     .map(|title| {
    //         iterate(
    //             wiki,
    //             Arc::clone(&graph),
    //             &mut rand::thread_rng(),
    //             title,
    //             breadth - 1,
    //             depth + 1,
    //         ).map_err(|err| std::fmt::write())
    //     })
    //     .collect::<Vec<Result<(), std::fmt::Error>>>();

    return Ok(());
}
