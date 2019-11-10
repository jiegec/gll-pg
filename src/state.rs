use petgraph::{graph::NodeIndex, Directed, Graph};
use std::collections::{BTreeMap, BTreeSet, VecDeque};

type GSSNode<L> = (L, usize);

#[derive(Debug)]
struct GSSState<L: Ord + Clone> {
    graph: Graph<GSSNode<L>, (), Directed>,
    nodes: BTreeMap<GSSNode<L>, NodeIndex>,
    visited: Vec<BTreeSet<(L, NodeIndex)>>,
    todo: VecDeque<(L, NodeIndex, usize)>,
    pop: BTreeSet<(NodeIndex, usize)>,
    accept_node_index: NodeIndex,
    current_node_index: NodeIndex,
}

impl<L: Ord + Clone> GSSState<L> {
    fn add(&mut self, l: L, u: NodeIndex, j: usize) {
        if !self.visited[j].contains(&(l.clone(), u)) {
            self.visited[j].insert((l.clone(), u));
            self.todo.push_back((l, u, j));
        }
    }

    fn pop(&mut self, u: NodeIndex, j: usize) {
        if u != self.accept_node_index {
            self.pop.insert((u, j));
            let node = self.graph[u].clone();
            let neighbors: Vec<NodeIndex> = self.graph.neighbors(u).collect();
            for v in neighbors {
                self.add(node.0.clone(), v, j);
            }
        }
    }

    fn create(&mut self, l: L, u: NodeIndex, j: usize) {
        let node = (l.clone(), j);
        let v = if let Some(index) = self.nodes.get(&node) {
            *index
        } else {
            let index = self.graph.add_node(node.clone());
            self.nodes.insert(node, index);
            index
        };
        if self.graph.find_edge(v, u).is_none() {
            self.graph.add_edge(v, u, ());
            let pop = self.pop.clone();
            for (index, k) in pop.iter() {
                if index == &v {
                    self.add(l.clone(), u, *k);
                }
            }
        }
        self.current_node_index = v;
    }
}


#[cfg(test)]
mod tests {
    use super::*;

    #[derive(Copy, Clone, Debug, Eq, PartialEq, Ord, PartialOrd)]
    enum Label {
        Accept,
        Ret,
        L0,
        LS,
        LS1,
        L1,
        L2,
        LS2,
        L3,
        L4,
        LS3,
        LA,
        LB,
    }

    #[test]
    fn parse_eps() {
        assert!(parse("$".as_bytes()));
    }

    fn parse(input: &[u8]) -> bool {
        use Label::*;
        let m = input.len() - 1;
        let mut i = 0;

        let mut graph: Graph<GSSNode<Label>, (), Directed> = Graph::new();
        let mut nodes = BTreeMap::new();
        let initial_node = (L0, 0);
        let accept_node = (Accept, 0);
        let initial_node_index = graph.add_node(initial_node);
        let accept_node_index = graph.add_node(accept_node);
        nodes.insert(initial_node, initial_node_index);
        nodes.insert(accept_node, accept_node_index);
        graph.add_edge(initial_node_index, accept_node_index, ());

        let mut state = GSSState {
            graph,
            nodes,
            visited: vec![BTreeSet::new(); input.len()],
            todo: VecDeque::new(),
            pop: BTreeSet::new(),
            accept_node_index,
            current_node_index: initial_node_index,
        };
        // FIRST(S$)
        if [b'a', b'b', b'c', b'd', b'$'].contains(&input[0]) {
            let mut current_label = LS;
            loop {
                match current_label {
                    L0 => {
                        if let Some((l, u, j)) = state.todo.pop_front() {
                            current_label = l;
                            state.current_node_index = u;
                            i = j;
                        } else {
                            if state.visited[m].contains(&(L0, accept_node_index)) {
                                return true;
                            } else {
                                return false;
                            }
                        }
                    }
                    LS => {
                        if [b'a', b'c'].contains(&input[i]) {
                            state.add(LS1, state.current_node_index, i);
                        }
                        if [b'a', b'c'].contains(&input[i]) {
                            state.add(LS2, state.current_node_index, i);
                        }
                        if true {
                            state.add(LS3, state.current_node_index, i);
                        }
                        current_label = L0;
                    }
                    LS1 => {
                        state.create(L1, state.current_node_index, i);
                        current_label = LA;
                    }
                    L1 => {
                        if [b'a', b'b', b'c', b'd', b'$'].contains(&input[i]) {
                            state.create(L2, state.current_node_index, i);
                            current_label = LB;
                        } else {
                            current_label = L0;
                        }
                    }
                    L2 => {
                        if input[i] == b'd' {
                            i += 1;
                            current_label = Ret;
                        } else {
                            current_label = L0;
                        }
                    }
                    LS2 => {
                        state.create(L3, state.current_node_index, i);
                        current_label = LB;
                    }
                    L3 => {
                        if [b'a', b'b', b'c', b'd', b'$'].contains(&input[i]) {
                            state.create(L4, state.current_node_index, i);
                            current_label = LS;
                        } else {
                            current_label = L0;
                        }
                    }
                    L4 => {
                        current_label = Ret;
                    }
                    LS3 => {
                        current_label = Ret;
                    }
                    LA => {
                        if [b'a', b'c'].contains(&input[i]) {
                            i += 1;
                            current_label = Ret;
                        } else {
                            current_label = L0;
                        }
                    }
                    LB => {
                        if [b'a', b'b'].contains(&input[i]) {
                            i += 1;
                            current_label = Ret;
                        } else {
                            current_label = L0;
                        }
                    }
                    Ret => {
                        state.pop(state.current_node_index, i);
                        current_label = L0;
                    }
                    Accept => unreachable!()
                }
            }
        } else {
            false
        }
    }
}

