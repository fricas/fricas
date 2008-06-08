#include "aldor.as"
#include "aldorio.as"


macro NodeType == Join(HashType, OutputType);

Edge(T: NodeType): Join(PrimitiveType, OutputType) with {
	edge: (T, T) -> %;
	p1: % -> T;
	p0: % -> T;
} == add {
	Rep == Record(p0: T, p1: T);
	import from Rep, String, T;
	(a: %) = (b: %): Boolean == p0 a = p0 b and p1 a = p1 b;
	edge(s: T, e: T): % == per [s, e];
	p0(e: %): T == (rep e).p0;
	p1(e: %): T == (rep e).p1;
	(p: TextWriter) << (e: %): TextWriter == p << p0 e << " -> " << p1 e;
}

define DirectedGraphCategory(T: NodeType): Category == OutputType with {
	nodes: % -> Generator T;
	edges: % -> Generator Edge T;

	outEdges: (%, T) -> List Edge T;
	inEdges:  (%, T) -> List Edge T;
	node?: (T, %) -> Boolean;
}

DirectedGraph(T: NodeType): DirectedGraphCategory T with {
	graph: List T -> %;
		++ graph(nodes) constructs a graph with the given nodes
		++ and no edges.
	graph: Generator T -> %;
		++ graph(nodes) constructs a graph with the given nodes
		++ and no edges.
	addLink!: (%, T, T) -> ();
		++ addLink!(g, t1, t2) destructively adds an edge from
		++ t1 to t2.
	ancestors: (%, T) -> Set T;
		++ ancestors(g, t) returns the ancestors of the node t
		++ in the graph g.
	#: % -> MachineInteger;
		++ #g returns the number of nodes in the graph g.
} == add {
	-- A graph is a collection triples of the form
	-- (node, inEdges, outEdges).
	-- That triples are represented in a hashtable where the node is
	-- the key and (inEdges, outEdges) is the value.
	NodeInfo == Record(inEdges: List Edge T, outEdges: List Edge T);
	Rep == HashTable(T, NodeInfo);
	import from NodeInfo, Rep;

	local emptyNodeInfo(): NodeInfo == [ [], [] ];
	#(g: %): MachineInteger == numberOfEntries rep(g);
	node?(node: T, g: %) : Boolean == {
		import from Partial NodeInfo;
		not failed? find(node, rep(g))
	}
	graph(nodes: List T): % == graph(node for node in nodes);
	graph(nodes: Generator T): % == {
		tbl: Rep := table();
		for node in nodes repeat tbl.node := emptyNodeInfo();
		per tbl
	}
	nodes(g: %): Generator T == keys rep g;
	edges(g: %): Generator Edge T == generate {
		for nodeinfo in entries rep g repeat {
			for inEdge in nodeinfo.inEdges repeat yield inEdge;
		}
	}
	-- Don't call if node?(n, g) is false!!!
	outEdges(g: %, n: T): List Edge T == rep(g).n.outEdges;
	inEdges(g: %, n: T):  List Edge T == rep(g).n.inEdges;

	addLink!(g: %, start: T, end: T): () == {
		import from String, TextWriter;
		import from Edge T, Partial NodeInfo;
		e := edge(start, end);
		TRACE("Add Link: ", e);
		if not node?(start, g) then { 
			stderr << "Missing start node: " << e << newline; 
			never;
		}
		if not node?(end, g) then {
			stderr << "Missing end node: " << e << newline; 
			never;
		}
		sInfo := (rep g).start;
		eInfo := (rep g).end;
		sInfo.outEdges := cons(e, sInfo.outEdges);
		eInfo.inEdges  := cons(e, eInfo.inEdges);
	}

	local ancVisit!(g: %, n: T, tbl: Set T): Set T == {
		import from Edge T, List Edge T;
		if member?(n, tbl) then return tbl;
		tbl := union!(tbl, n);
		for inEdge in inEdges(g, n) repeat {
			tbl := ancVisit!(g, p0 inEdge, tbl);
		}
		return tbl;
	}
	ancestors(g: %, n: T): Set T == ancVisit!(g, n, empty);

	(o: TextWriter) << (g: %): TextWriter == { 
		import from T, List T, Edge T, String; 
		o << "[G: ";
		for node in nodes g repeat o << node << ", ";
		o << "]";
	}
}


GraphAlgorithms(T: NodeType): with {
	connectedComponents: DirectedGraph T-> Set Set T;
} == add {
	-- components that are strongly connected (ie. path from every
	-- elt to every other)
	connectedComponents(g: DirectedGraph T): Set Set T == {
		import from Edge T, List Edge T;
		local components: Set Set T := [];
		local fwdTbl: Set T := [];
		local backTbl: Set T := [];
		local visitOrder: List T := [];

		fwdVisit(node: T): ()  == {
			free fwdTbl := union!(fwdTbl, node);
			for outEdge in outEdges(g, node) repeat {
				nxtNode := p1 outEdge;
				if not member?(nxtNode, fwdTbl) then {
					fwdVisit nxtNode;
				}
			}
			free visitOrder := cons(node, visitOrder);
		}

		backVisit(node: T): Set T  == {
			free backTbl := union!(backTbl, node);
			mbrs: Set T := [node];
			for inEdge in inEdges(g, node) repeat {
				nxtNode := p0 inEdge;
				if not member?(nxtNode, backTbl) then {
					mbrs := union!(mbrs,backVisit nxtNode);
				}
			}
			mbrs
		}

		-- /* First, do nodes forwards */
		for node in nodes g | not member?(node, fwdTbl) repeat {
			fwdVisit node;
		}
	  
		-- /* And backwards */
		for node in visitOrder | not member?(node, backTbl) repeat {
			components := union!(components, backVisit node);
		}
		return components;
	}
}


-------------------------------------------------------------------
-- addlink!(g, dep, id) means that id depends on dep.

-- Mail of Peter Broadbery 26-May-2008
--------------------------------------
-- The idea is that we break the library up into things that the
-- axextend/axlit depends on, then all the rest.  The reason is that
-- axextend/axlit supplies the literal methods which are required for
-- compilation of some axiom files - knowledge of that could be built in
-- to genax.lsp, but life's too short sometimes.

-- Mail of Peter Broadbery 27-May-2008
-------------------------------------- 
-- baselist.lsp is more about simplifying the earlier parts of the
-- compile order. The definitions in baselist.lsp are chosen to
-- a) simplify the dependency graph - eg. nothing cares about what
--    OMDEV does, simply that it's a domain. The extension can come anytime.
-- b) satisfy constraints on uses of domains in categories - for example,
--    the definition of DIVRING implies that ALGEBRA FRAC INT is a legal
--    expression. This implies that INT must export INTDOM, and FRAC
--    must produce at least a commutative ring (baselist says field,
--    which is what it ultimately will be anyway).
-- Without this, processing the normal dependencies results in a cycle
-- of 40 or so files which is too many for aldor to handle well.

MakefileGeneration: with {
	generateMakefile: (String, String) -> ();
		++ generateMakefile(libaxiom.lst, extradeps.lst)
		++ writes a Makefile to stdout.
} == add {
	macro S == String;
	macro G == DirectedGraph S;

        generateMakefile(libaxiomlst: S, extradepslst: S): () == {
		TRACE("generateMakefile: ", "Enter");
		import from MachineInteger, S, List S;
		files:     List S := [readNames libaxiomlst];
		depsPairs: List S := [readNames extradepslst];

		fileMap: HashTable(S, List S) := table();
		fileMap."base"      := [];
		fileMap."axiom"     := [];
		fileMap."initaxiom" := [];
		fileMap."aldorext"  := [];

		allTypes: List S := []; -- does not contain duplicates
		
		-- Read libaxiom.lst.
		for pair in extractPairs files repeat {
			(class, ident) := pair;
			fileMap.class := cons(ident, fileMap.class);
			allTypes := cons(ident, allTypes);
		}
		allTypes := sort! allTypes;
		TRACE("AllTypes: ", allTypes);

		g: G := graph allTypes;

		-- Every domain should depend on the aldor base domains.
		ids: List S := [];
		ids := append!(ids, copy fileMap."initaxiom");
		ids := append!(ids, copy fileMap."axiom");
		ids := append!(ids, copy fileMap."aldorext");
		for id in ids repeat addDependencies!(g, fileMap."base", id);

		-- "T" depends on "init_T"
		for id in fileMap."initaxiom" repeat {
			addDependency!(g, id, substring(id, 5));
		}

		addReadDependencies!(g, fileMap."initaxiom");

		
		-- Now there are two kinds of types:
		-- lower types and upper types
		-- A type is a lower type if it is a dependency of
		-- axlit.as or axextend.as. That naturally includes all
		-- the elements of fileMap."base" and fileMap."initAxiom".
		-- A type is an upper type if it is not a lower type.
		-- In the terminology of this program, a type corresponds
		-- to a filename. So also axlit and lang are considered
		-- to be types.
		
		depTypes: Set S := [];
		-- fileMap."aldorext" contains axlit and axextend.
		for id in fileMap("aldorext") repeat {
			depTypes := union!(depTypes, [readDependencies id]);
		}
                -- depTypes = ABELSG BOOLEAN EXIT INT NNI
		--            ORDSET RING SETCAT SINT TUPLE TYPE
		TRACE("depTypes: ", sort! [generator depTypes]);
		
		local a: G := graph fileMap."axiom";
		addReadDependencies!(a, fileMap."axiom");
		lowerTypes: List S := sort! [
		    ancestors(a, generator depTypes)
		];
		TRACE("lowerTypes: ", lowerTypes);
		for id in lowerTypes repeat {
			for dep in readDependencies id repeat {
				idep := "init__" + dep;
				if member?(idep, fileMap."initaxiom") then {
					addDependency!(g, idep, id);
				} else {
					addDependency!(g, dep, id);
				}
			}
		}
		addReadDependencies!(g, fileMap."aldorext");
		TRACE("after aldorext: ", "");

		-- The types that axextend.as and axlit.as do not
		-- (recursively) depend on. So we make them depend
		-- on axextend and axlit.
		upperTypes: List S := [
		    s for s in allTypes | 
		      not member?(s, lowerTypes) and
		      not member?(s, fileMap."base") and
		      not member?(s, fileMap."initaxiom") and
		      not member?(s, fileMap."aldorext")
		];

		TRACE("upperTypes: ", upperTypes);
		for id in upperTypes repeat {
			TRACE("uid: ", id);
			addDependencies!(g, fileMap."aldorext", id);
		}
		TRACE("now all upper types depend on aldorext", "");
		-- Now add the dependencies of the upper types.
		addReadDependencies!(g, upperTypes);
		TRACE("all dependencies of upper types have been added", "");

		-- Finally, add the extra dependencies
		for pair in extractPairs depsPairs repeat {
			import from List S;
			(ident, dep) := pair;
			TRACE("[id, dep]  = ", [ident, dep]$List(S));
			addDependency!(g, dep, ident);
		}

		import from GraphAlgorithms(S), Set S, Set Set S;
		-- Sorting is not really necessary. We do it only to ease
		-- checking against previous versions of the output.
		s: List List S := [
		    sort! [id for id in clq] for clq in connectedComponents g
		];
		s := sort!(s, smaller);
		TRACE("ConnectedComponents: ", s);     
		writeComponents(g, s);
	}

	-- The following function is never applied to empty lists.
	local smaller(x: List S, y: List S): Boolean == {
		import from MachineInteger, S;
		#x < #y or (#x = #y and first x < first y);
	}

	local writeComponents(g: G, s: List List S): () == {
		import from List S, Edge S, List Edge S;
		out: TextWriter := stdout;
		nl: Character := newline;
		out << "CLIQUES := " << newline;
		
		-- Each clique gets a name. The name is taken to be the
		-- lexicographically smallest name of the clique members.
		-- Since the entries in the clique are sorted, we simply
		-- take the first name. clqid can be seen as a mapping from
		-- the member name of a clique to the name of the clique.
		clqid: HashTable(S, S) := table();
		for clq in s repeat for id in clq repeat clqid.id := first clq;
		for clq in s repeat {
			name: S := first clq;
			TRACE("write--------------: ", name);
			out << "CLIQUES += " << name <<nl;
			out << "CLIQUE__MEMBERS__" << name << " =";
			for id in clq repeat out << " " << id;
			out << nl;

			-- Write out direct dependencies.
			out << "CLIQUE__DEPS__" << name+" =";
			depNodes: Generator S := generate {
				for id in clq repeat {
					for e in inEdges(g, id) repeat {
						yield p0 e
					}
				}
			}
			depNodesRed: Set S := [
			    id for id in depNodes | not member?(id, clq)
			];
			TRACE("write-depNodesRed: ", depNodesRed);
			deps: Set S := [clqid.id for id in depNodesRed];
			sdeps: List S := sort! [generator deps];
			TRACE("write-deps: ", sdeps);
			for id in sdeps repeat out << " " + id;
			out << nl;

			-- Write out all (recursive) dependencies.
			out << "CLIQUE__ALLDEPS__" << name << " =";
			depNodes: Generator S := ancestors(g, generator clq);
			depNodesRed: Set S := [
			    id for id in depNodes | not member?(id, clq)
			];
			TRACE("write-alldepNodesRed: ", depNodesRed);
			deps: Set S := [clqid.id for id in depNodesRed];
			sdeps: List S := sort! [generator deps];
			TRACE("write-alldeps: ", sdeps);
			for id in sdeps repeat out << " " + id;
			out << nl;

			-- Write out (recursive) dependencies in
			-- Makefile format.
			out << "ap/" << name << ".ap: ";
			out << "$(patsubst %,ap/%.ap,";
			out << "$(CLIQUE__ALLDEPS__" << name << "))" << nl;

			libname := "al/libaxiom__" + name + ".al";
			
			out << libname << ": ";
			out << "$(patsubst %," << libname << "(%.ao)";
			out << ", $(CLIQUE__ALLDEPS__" << name << "))" << nl;

			out << libname << "(%.ao): ao/%.ao" << nl;
			out << tab << "ar r $@ $<" << nl;
		}
	}

	-- Return the union of all recursive ancestors of 'types'.
	-- This function is only used to figure out the lower types.
	local ancestors(g: G, types: Generator S): Generator S == {
		ancs: Set S := [];
		for id in types repeat {
			ancs := insert!(id, union!(ancs, ancestors(g, id)));
		}
		return generator ancs;
	}

	local addDependency!(g: G, dep: S, idx: S): () == {
		node?(dep, g) => addLink!(g, dep, idx);
		stderr << "Missing node: [" << dep << "]" << newline;
		never;
	}
	local addDependencies!(g: G, deps: Generator S, idx: S): () == {
		for dep in deps repeat addDependency!(g, dep, idx);
	}
	local addDependencies!(g: G, deps: List S, idx: S): () == {
		addDependencies!(g, generator deps, idx);
	}
	local addReadDependencies!(g: G, idx: S): () == {
		addDependencies!(g, readDependencies idx, idx);
	}
	local addReadDependencies!(g: G, ids: List S): () == {
		for id in ids repeat addReadDependencies!(g, id);
	}

        local extractPairs(l: List S): Generator Cross(S, S) == {
		extractPairs(s for s in l);
	}

	local extractPairs(l: Generator S): Generator Cross(S, S) == {
		extractPair s for s in l;
	}

	-- We assume that there is exactly one space in the string and
	-- no additional space at the end.
	local extractPair(s: S): Cross(S, S) == {
		p: MachineInteger := 0;
		for i in 0..#s-1 repeat if space?(s.i) then p := i;
		(substring(s, 0, p), substring(s, p+1))
	}

	local readDependencies(f: S): Generator S == {
		readNames("gendeps/" + f + ".dep");
	}
	local readNames(fname: S): Generator S == generate {
		--TRACE("readNames: {", fname);
		import from MachineInteger;
		local sb: StringBuffer;
		f: File := open fname;
		rdr: TextReader := coerce f;
		while (readUntil!(sb:=new(), rdr, newline) = newline) repeat {
			yield string sb;
		}
		close! f;
		--TRACE("readNames: }", fname);
	}
}

main(): () == {
	import from MachineInteger, MakefileGeneration;
	args: Array String := arguments$CommandLine;
	TRACE("BEGIN clq: ", args);
	generateMakefile(args.0, args.1);
	TRACE("END clq: ", args);
}

main();



