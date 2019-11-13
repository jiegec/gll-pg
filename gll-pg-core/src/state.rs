//! GLL Grammar state and SPPF structure

use petgraph::{
    dot::Dot,
    graph::{EdgeReference, NodeIndex},
    visit::EdgeRef,
};
/// Re-exported `petgraph` structs to avoid requiring dependency of generated code on it.
pub use petgraph::{Directed, Graph};
use std::collections::{BTreeMap, BTreeSet};
use std::fmt::{Debug, Write};

/// GSS Node Label Type
pub type GSSNode<L> = (L, usize);

/// SPPF Nodes are stored into a vec, and references are stored as integer
pub type SPPFNodeIndex = usize;

/// A common trait that all symbols should impl,
/// the symbols and impl are generated.
/// You don't need to impl it in your code.
pub trait GrammarSymbol {
    fn is_eps(&self) -> bool;
}

/// A common trait that all labels  should impl,
/// the labels and impl are generated.
/// You don't need to impl it in your code.
pub trait GrammarLabel {
    type Symbol: PartialEq + GrammarSymbol;
    /// if self is of form `X ::= a . b`,
    /// return true if a is a terminal or a non-nullable nonterminal and if b is not eps
    fn first(&self) -> bool;

    /// return Some(lhs) if it is the end of a grammar rule `lhs -> ...`, otherwise None
    fn end(&self) -> Option<Self::Symbol>;
}

/// Binary SPPF Node structure.
///
/// Each node can be one of: Dummy, Symbol, Intermediate and Packed.
///
/// Symbol is a Terminal or a Nonterminal.
///
/// Intermediate is a grammar rule with position.
///
/// Packed means different derivations of the same grammar rule.
///
/// Each grammar rule possible position corresponds to a label.
/// So store labels for Intermediate and Packed rules.
#[derive(Debug, Clone, Eq, PartialEq, Ord, PartialOrd)]
pub enum SPPFNode<L, S> {
    /// $ node in original paper
    Dummy,
    /// (symbol, left, right, children)
    Symbol(S, usize, usize, Vec<SPPFNodeIndex>),
    /// (label, left, right, children)
    Intermediate(L, usize, usize, Vec<SPPFNodeIndex>),
    /// (label, split, children)
    Packed(L, usize, Vec<SPPFNodeIndex>),
}

/// All GSS Parser states.
/// It is used by generated code, don't use it directly.
#[derive(Debug)]
pub struct GSSState<L: Ord + Clone + GrammarLabel> {
    /// Direct GSS graph
    pub graph: Graph<GSSNode<L>, SPPFNodeIndex, Directed>,
    /// Mapping from node to its index
    pub nodes: BTreeMap<GSSNode<L>, NodeIndex>,
    /// All sppf nodes, and nodes reference each other by index
    pub sppf_nodes: Vec<SPPFNode<L, L::Symbol>>,
    pub initial_node_index: NodeIndex,
    /// U_j in original paper
    pub visited: Vec<BTreeSet<(L, NodeIndex, SPPFNodeIndex)>>,
    /// R in original paper
    pub todo: Vec<(L, NodeIndex, usize, SPPFNodeIndex)>,
    /// P in original paper
    pub pop: BTreeSet<(NodeIndex, SPPFNodeIndex)>,
    /// C_i in original paper
    pub current_position: usize,
    /// C_u in original paper
    pub current_node_index: NodeIndex,
    /// C_n in original paper
    pub current_sppf_node: usize,
}

impl<L, S> SPPFNode<L, S> {
    /// Get right extent of the node, panics if it doesn't have it.
    pub fn right_extent(&self) -> usize {
        use SPPFNode::*;
        match self {
            Symbol(_, _, r, _) => *r,
            Intermediate(_, _, r, _) => *r,
            _ => panic!("no right extent for packed and dummy"),
        }
    }

    /// Get left extent of the node, panics if it doesn't have it.
    pub fn left_extent(&self) -> usize {
        use SPPFNode::*;
        match self {
            Symbol(_, l, _, _) => *l,
            Intermediate(_, l, _, _) => *l,
            _ => panic!("no left extent for packed and dummy"),
        }
    }

    /// Get children node references.
    pub fn children(&self) -> Option<&Vec<SPPFNodeIndex>> {
        use SPPFNode::*;
        match self {
            Dummy => None,
            Symbol(_, _, _, children) => Some(children),
            Intermediate(_, _, _, children) => Some(children),
            Packed(_, _, children) => Some(children),
        }
    }

    fn children_mut(&mut self) -> Option<&mut Vec<SPPFNodeIndex>> {
        use SPPFNode::*;
        match self {
            Symbol(_, _, _, children) => Some(children),
            Intermediate(_, _, _, children) => Some(children),
            _ => panic!("no children for packed and dummy"),
        }
    }
}

impl<L: Ord + Clone + GrammarLabel + Debug> GSSState<L> {
    /// Print current GSS graph in graphviz format
    pub fn print_gss_dot(&self) -> String {
        format!("{:?}", Dot::with_config(&self.graph, &[]))
    }
}

impl<L: Ord + Clone + GrammarLabel> GSSState<L>
where
    <L as GrammarLabel>::Symbol: Debug,
{
    /// Print current SPPF graph in graphviz format
    pub fn print_sppf_dot(&self) -> String {
        let mut res = String::new();
        write!(&mut res, "digraph {{\n").unwrap();
        for (i, node) in self.sppf_nodes.iter().enumerate() {
            let label = match node {
                SPPFNode::Symbol(s, _, _, _) => format!("{:?}", s),
                SPPFNode::Intermediate(_, _, _, _) => format!("I"),
                SPPFNode::Packed(_, _, _) => format!("P"),
                SPPFNode::Dummy => format!("D"),
            };
            write!(&mut res, "{} [label={:?}]\n", i, label).unwrap();
            if let Some(children) = node.children() {
                for child in children {
                    write!(&mut res, "{} -> {}\n", i, child).unwrap();
                }
            }
        }
        write!(&mut res, "}}").unwrap();
        res
    }
}

impl<L: Ord + Clone + GrammarLabel> GSSState<L> {
    /// The `add` function in the paper
    pub fn add(&mut self, l: L, u: NodeIndex, i: usize, w: SPPFNodeIndex) {
        if !self.visited[i].contains(&(l.clone(), u, w)) {
            self.visited[i].insert((l.clone(), u, w));
            self.todo.push((l, u, i, w));
        }
    }

    /// The `pop` function in the paper
    pub fn pop(&mut self, u: NodeIndex, i: usize, z: SPPFNodeIndex) {
        if u != self.initial_node_index {
            let (l, _k) = self.graph[u].clone();
            self.pop.insert((u, z));
            let edges: Vec<EdgeReference<SPPFNodeIndex>> = self.graph.edges(u).collect();
            let edge_data: Vec<(NodeIndex, SPPFNodeIndex)> = edges
                .iter()
                .map(|edge| (edge.target(), *edge.weight()))
                .collect();
            for (v, w) in edge_data {
                let y = self.get_node_p(l.clone(), w, z);
                self.add(l.clone(), v, i, y);
            }
        }
    }

    /// The `create` function in the paper
    pub fn create(&mut self, l: L, u: NodeIndex, j: usize, w: SPPFNodeIndex) -> NodeIndex {
        let node = (l.clone(), j);
        let v = if let Some(index) = self.nodes.get(&node) {
            *index
        } else {
            let index = self.graph.add_node(node.clone());
            self.nodes.insert(node, index);
            index
        };
        if self.graph.find_edge(v, u).is_none() {
            self.graph.add_edge(v, u, w);
            let pop = self.pop.clone();
            for (index, z) in pop.into_iter() {
                if index == v {
                    let y = self.get_node_p(l.clone(), w, z);
                    let h = self.sppf_nodes[z].right_extent();
                    self.add(l.clone(), u, h, y);
                }
            }
        }
        v
    }

    /// The `get_node_t` function in the paper
    pub fn get_node_t(&mut self, x: L::Symbol, i: usize) -> SPPFNodeIndex {
        let h = if x.is_eps() { i } else { i + 1 };
        self.find_or_create_sppf_symbol(x, i, h)
    }

    /// The `get_node_p` function in the paper
    pub fn get_node_p(&mut self, l: L, w: SPPFNodeIndex, z: SPPFNodeIndex) -> SPPFNodeIndex {
        if l.first() {
            return z;
        } else {
            let node_z = &self.sppf_nodes[z];
            let k = node_z.left_extent();
            let i = node_z.right_extent();
            let node_w = &self.sppf_nodes[w];
            if SPPFNode::Dummy != *node_w {
                // w != $
                let j = node_w.left_extent();
                assert_eq!(node_w.right_extent(), k);
                if let Some(t) = l.end() {
                    // t = X
                    let y = self.find_or_create_sppf_symbol(t, j, i);
                    if let Some(children) = self.sppf_nodes[y].children() {
                        if !children.iter().any(|index| match &self.sppf_nodes[*index] {
                            SPPFNode::Packed(node_l, node_k, _) => *node_l == l && *node_k == k,
                            _ => false,
                        }) {
                            let len = self.sppf_nodes.len();
                            self.sppf_nodes[y].children_mut().unwrap().push(len);
                            self.sppf_nodes.push(SPPFNode::Packed(l, k, vec![w, z]));
                        }
                    } else {
                        unreachable!()
                    }
                    y
                } else {
                    // t = l
                    let y = self.find_or_create_sppf_intermediate(l.clone(), j, i);
                    if let Some(children) = self.sppf_nodes[y].children() {
                        if !children.iter().any(|index| match &self.sppf_nodes[*index] {
                            SPPFNode::Packed(node_l, node_k, _) => *node_l == l && *node_k == k,
                            _ => false,
                        }) {
                            let len = self.sppf_nodes.len();
                            self.sppf_nodes[y].children_mut().unwrap().push(len);
                            self.sppf_nodes.push(SPPFNode::Packed(l, k, vec![w, z]));
                        }
                    } else {
                        unreachable!()
                    }
                    y
                }
            } else {
                // w = $
                if let Some(t) = l.end() {
                    // t = X
                    let y = self.find_or_create_sppf_symbol(t, k, i);
                    if let Some(children) = self.sppf_nodes[y].children() {
                        if !children.iter().any(|index| match &self.sppf_nodes[*index] {
                            SPPFNode::Packed(node_l, node_k, _) => *node_l == l && *node_k == k,
                            _ => false,
                        }) {
                            let len = self.sppf_nodes.len();
                            self.sppf_nodes[y].children_mut().unwrap().push(len);
                            self.sppf_nodes.push(SPPFNode::Packed(l, k, vec![z]));
                        }
                    } else {
                        unreachable!()
                    }
                    y
                } else {
                    // t = l
                    let y = self.find_or_create_sppf_intermediate(l.clone(), k, i);
                    if let Some(children) = self.sppf_nodes[y].children() {
                        if !children.iter().any(|index| match &self.sppf_nodes[*index] {
                            SPPFNode::Packed(node_l, node_k, _) => *node_l == l && *node_k == k,
                            _ => false,
                        }) {
                            let len = self.sppf_nodes.len();
                            self.sppf_nodes[y].children_mut().unwrap().push(len);
                            self.sppf_nodes.push(SPPFNode::Packed(l, k, vec![z]));
                        }
                    } else {
                        unreachable!()
                    }
                    y
                }
            }
        }
    }

    fn find_or_create_sppf_symbol(&mut self, s: L::Symbol, i: usize, j: usize) -> SPPFNodeIndex {
        for (index, node) in self.sppf_nodes.iter().enumerate() {
            if let SPPFNode::Symbol(node_s, node_i, node_j, _) = node {
                if *node_s == s && *node_i == i && *node_j == j {
                    return index;
                }
            }
        }
        self.sppf_nodes.push(SPPFNode::Symbol(s, i, j, vec![]));
        self.sppf_nodes.len() - 1
    }

    fn find_or_create_sppf_intermediate(&mut self, l: L, i: usize, j: usize) -> SPPFNodeIndex {
        for (index, node) in self.sppf_nodes.iter().enumerate() {
            if let SPPFNode::Intermediate(node_l, node_i, node_j, _) = node {
                if *node_l == l && *node_i == i && *node_j == j {
                    return index;
                }
            }
        }
        self.sppf_nodes
            .push(SPPFNode::Intermediate(l, i, j, vec![]));
        self.sppf_nodes.len() - 1
    }

    /// Collect all symbol leaves of a packed node
    pub fn collect_symbols(&self, node: SPPFNodeIndex) -> Vec<SPPFNodeIndex> {
        use SPPFNode::*;
        match &self.sppf_nodes[node] {
            Dummy => vec![],
            Symbol(_, _, _, _) => vec![node],
            Intermediate(_, _, _, children) => children
                .iter()
                .map(|node| self.collect_symbols(*node))
                .flatten()
                .collect(),
            Packed(_, _, children) => children
                .iter()
                .map(|node| self.collect_symbols(*node))
                .flatten()
                .collect(),
        }
    }
}
