mod gll_generated {

    #[derive(Copy, Clone, Debug, Eq, PartialEq, Ord, PartialOrd)]
    pub enum Label {
        Ret,
        L0,
        
    }

    #[derive(Copy, Clone, Debug, Eq, PartialEq, Ord, PartialOrd)]
    pub enum Symbol {
        
        
        Eps,
    }

    impl gll_pg_core::GrammarSymbol for Symbol {
        fn is_eps(&self) -> bool {
            *self == Symbol::Eps
        }
    }

    impl gll_pg_core::GrammarLabel for Label {
        type Symbol = Symbol;

        fn first(&self) -> bool {
            use Label::*;
            [].contains(self)
        }

        fn end(&self) -> Option<Symbol> {
            use Label::*;
            use Symbol::*;
            match self {
                
                _ => None,
            }
        }
    }
}

pub struct Parser { }

impl Parser {
    pub fn parse(lexer: &mut logos::Lexer<Token, &str>) -> Option<isize> {
        use gll_pg_core::*;
        use gll_generated::Label::*;
        let mut input = vec![];
        loop {
            if lexer.token == Token::End {
                break;
            }
            if lexer.token == Token::_Eps {
                lexer.advance();
                continue;
            }
            input.push(lexer.token.clone());
            lexer.advance();
        }
        println!("{:?}", input);
        let m = input.len() - 1;
        let mut graph: Graph<GSSNode<gll_generated::Label>, SPPFNodeIndex, Directed> = Graph::new();
        let mut nodes = BTreeMap::new();
        let initial_node = (L0, 0);
        let initial_node_index = graph.add_node(initial_node);
        nodes.insert(initial_node, initial_node_index);

        let mut state = GSSState {
            graph,
            nodes,
            sppf_nodes: vec![SPPFNode::Dummy],
            initial_node_index,
            visited: vec![BTreeSet::new(); input.len()],
            todo: Vec::new(),
            pop: BTreeSet::new(),
            current_node_index: initial_node_index,
            current_sppf_node: 0,
            current_position: 0,
        };
        None
    }
}