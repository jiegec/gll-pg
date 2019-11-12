pub use petgraph::{
    graph::{EdgeReference, NodeIndex},
    visit::EdgeRef,
    Directed, Graph,
};
pub use std::collections::{BTreeMap, BTreeSet, VecDeque};

pub type GSSNode<L> = (L, usize);
pub type SPPFNodeIndex = usize;

pub trait GrammarSymbol {
    fn is_eps(&self) -> bool;
}

pub trait GrammarLabel {
    type Symbol: PartialEq + GrammarSymbol;
    fn first(&self) -> bool;
    // return Some(lhs) if it is the end
    fn end(&self) -> Option<Self::Symbol>;
}

#[derive(Debug, Clone, Eq, PartialEq, Ord, PartialOrd)]
pub enum SPPFNode<L, S> {
    Dummy,
    // usize, usize: from, to
    // Vec<SPPFNodeIndex>: children
    Symbol(S, usize, usize, Vec<SPPFNodeIndex>),
    Intermediate(L, usize, usize, Vec<SPPFNodeIndex>),
    Packed(L, usize, Vec<SPPFNodeIndex>),
}

#[derive(Debug)]
pub struct GSSState<L: Ord + Clone + GrammarLabel> {
    pub graph: Graph<GSSNode<L>, SPPFNodeIndex, Directed>,
    pub nodes: BTreeMap<GSSNode<L>, NodeIndex>,
    pub sppf_nodes: Vec<SPPFNode<L, L::Symbol>>,
    pub initial_node_index: NodeIndex,
    pub visited: Vec<BTreeSet<(L, NodeIndex, SPPFNodeIndex)>>, // U_j
    pub todo: Vec<(L, NodeIndex, usize, SPPFNodeIndex)>,       // R
    pub pop: BTreeSet<(NodeIndex, SPPFNodeIndex)>,             // P
    pub current_position: usize,                               // C_i
    pub current_node_index: NodeIndex,                         // C_u
    pub current_sppf_node: usize,                              // C_n
}

impl<L, S> SPPFNode<L, S> {
    fn right_extent(&self) -> usize {
        use SPPFNode::*;
        match self {
            Symbol(_, _, r, _) => *r,
            Intermediate(_, _, r, _) => *r,
            _ => unimplemented!(),
        }
    }

    fn left_extent(&self) -> usize {
        use SPPFNode::*;
        match self {
            Symbol(_, l, _, _) => *l,
            Intermediate(_, l, _, _) => *l,
            _ => unimplemented!(),
        }
    }

    fn children(&self) -> Option<&Vec<SPPFNodeIndex>> {
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
            _ => unimplemented!(),
        }
    }
}

impl<L: Ord + Clone + GrammarLabel> GSSState<L> {
    pub fn add(&mut self, l: L, u: NodeIndex, i: usize, w: SPPFNodeIndex) {
        if !self.visited[i].contains(&(l.clone(), u, w)) {
            self.visited[i].insert((l.clone(), u, w));
            self.todo.push((l, u, i, w));
        }
    }

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

    pub fn get_node_t(&mut self, x: L::Symbol, i: usize) -> SPPFNodeIndex {
        let h = if x.is_eps() { i } else { i + 1 };
        self.find_or_create_sppf_symbol(x, i, h)
    }

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
                        unimplemented!()
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
                        unimplemented!()
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
                        unimplemented!()
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
                        unimplemented!()
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
}
