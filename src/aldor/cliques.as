-- This program is responsible for the generation of cliques.mk.
-- It takes as input a file libaxiom.lst and the dependency files
-- from the 'gendeps' directory.

-- Copyright (C) 2005-2008,  Peter Broadbery
-- Copyright (C) 2008,  Ralf Hemmecke

-- The program was originally written by Peter Broadbery.
-- Ralf Hemmecke extended the program to write out dependencies
-- in actual compilation order and thus make the compilation process
-- of libaxiom.al easier. There is no longer any need to generate
-- .lst files so that libaxiom.al contains the .ao files in an
-- appropriate order (later files only depend on earlier files).

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


+++ A clique is a collection of names of types that have to be
+++ compiled together because of their interdependencies.
Clique: OutputType with {
	name: % -> String;
		++ returns the name of the clique;
	bracket: Generator String -> %;
		++ Creates a clique with associated number zero.
	generator: % -> Generator String;
		++ Generate the members in lexicographical order.
	setDependencies!: (%, List %) -> ();
		++ Attaches dependencies of the clique.
	dependencies: % -> List %;
		++ Returns the dependencies of the clique in compilation
		++ order.
	setIndex!: (%, MachineInteger) -> ();
		++ Attaches a number to a clique.
	smallerSize?: (%, %) -> Boolean;
		++ A clique x is smaller than a clique y if either it
		++ has smaller size or its name is lexicographically
		++ smaller.
	compileEarlier?: (%, %) -> Boolean;
		++ A clique x must be compiled earlier than a clique y,
		++ if x is a dependency of y or neither x is a dependency
		++ of y nor y is a dependency of x and x is
		++ lexicographically smaller than y.
	smallerIndex?: (%, %) -> Boolean;
		++ Just compares the indices of the cliques.
		++ The idea is that the indices correspond to
		++ indices in list that is sorted by compileEarlier?.
		++ So this test is faster.
} == add {
	macro S == String;
	Rep == Record(
	    mbrs: List S,       --members
	    deps: List %,       --dependencies
	    depNames: List S,   --dependency names
	    idx: MachineInteger --compilation order
	);
	import from Rep, S, List S, MachineInteger;
	-- The entries are supposed to be alphabetically sorted.
	members(x: %): List S == rep(x).mbrs;
	name(x: %): S == first members x;
	bracket(g: Generator S): % == per [sort! [g], empty, empty, 0];
	generator(x: %): Generator S == generator members x;
	#(x: %): MachineInteger == # members x;
	setDependencies!(x: %, d: List %): () == {
		rep(x).deps := d;
		rep(x).depNames := [name clq for clq in d];
	}
	dependencies(x: %): List % == rep(x).deps;
	setIndex!(x: %, i: MachineInteger): () == rep(x).idx := i;
	-- The following function is never applied to empty lists.
	smallerSize?(x: %, y: %): Boolean == {
		#x < #y or (#x = #y and name x < name y);
	}
	local dependencyNames(x: %): List S == rep(x).depNames;
	compileEarlier?(x: %, y: %): Boolean == {
		member?(name x, dependencyNames y) => true;
		member?(name y, dependencyNames x) => false;
		name x < name y;
	}
	-- idx should have been set before
	smallerIndex?(x: %, y: %): Boolean == rep(x).idx < rep(y).idx;
	(tw: TextWriter) << (x: %): TextWriter == {
		tw << "Clq(" << rep(x).idx << "-" << members x << ")";
	}
}


+++ Cliques represents a collection of cliques, i.e, a collection of
+++ connected components.
Cliques: OutputType with {
	bracket: DirectedGraph String -> %;
		++ Creates cliques from a directed graph.
	generator: % -> Generator Clique;
		++ generates the cliques in compilation order
	sizeSorted: % -> List Clique;
		++ generates the cliques in size+lex order
} == add {
	macro {
		S  == String;
		G  == DirectedGraph S;
	}
	Rep == List Clique;
	import from Rep, List S, Clique;
	sizeSorted(x: %): List Clique == sort!(copy rep x, smallerSize?);
	-- Important! The sort! from 'List' does not work for the
	-- sort to compilation order, since that
	local insertClique!(c: Clique, previous: List Clique): () == {
		cur := rest previous; -- pointer to current entry
		while not empty? cur and compileEarlier?(first cur, c) repeat {
			previous := cur;
			cur := rest cur;
		}
		setRest!(previous, cons(c, cur));
	}
	local compileOrder(cliques: List Clique): List Clique == {
		cliques: List Clique := sizeSorted per cliques;
		z: List Clique := [first cliques]; -- dummy value, never used
		for c in cliques repeat insertClique!(c, z);
		rest z; -- remove dummy value
	}
	bracket(g: G): % == {
		import from GraphAlgorithms(S), Set S, Set Set S, Clique;
		cliques: List Clique := [
		    [generator cc] for cc in connectedComponents g
		];
		local id2clq: HashTable(S, Clique) := table();
		for clq in cliques repeat {
			for id in clq repeat id2clq.id := clq;
		}
		-- There are no dependencies, yet. We set it now for each
		-- clique.
		for clq in cliques repeat {
			a: Set S := []; -- all dependencies
			for id in clq repeat a := union!(a, ancestors(g, id));
			-- use the name of the clique instead of the type name
			deps: Set S := [name(id2clq.id) for id in a];
			-- now remove self dependencies and sort
			n := name clq;
			sdeps := sort!([id for id in deps | id ~= n]$List(S));
			setDependencies!(clq, [id2clq.id for id in sdeps]);
		}
		-- The cliques should be topologically sorted according
		-- to the dependencies.
		-- We topologically sort the dependencies. (This step
		-- is not really necessary, since 'make' takes care
		-- of the actual order. So the sort is just for debugging
		-- purposes.)
		cliques := compileOrder cliques;
		-- remember the index for later
		import from MachineInteger;
		for clq in cliques for i in 1.. repeat setIndex!(clq, i);
		
		-- reset dependencies to compilation order
		for clq in cliques repeat {
			sorteddeps := sort!(dependencies clq, smallerIndex?);
			setDependencies!(clq, sorteddeps);
		}
		per cliques;
	}
	generator(x: %): Generator Clique == generator rep x;
	(tw: TextWriter) << (x: %): TextWriter == {
		import from List Clique;
		tw << [generator x];		
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
	generateMakefile: String -> ();
		++ generateMakefile(libaxiom.lst)
		++ writes a Makefile to stdout.
} == add {
	macro S  == String;
	macro G  == DirectedGraph S;
	out: TextWriter := stdout;
	nl: Character := newline;

        generateMakefile(libaxiomlst: S): () == {
		TRACE("generateMakefile: ", "Enter");
		import from MachineInteger, S, List S;
		files: List S := [readNames libaxiomlst];

		fileMap: HashTable(S, List S) := table();
		fileMap."lang"      := [];
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
		addReadDependencies!(g, allTypes);

		-- First, add the extra dependencies.
		addDependency!(g, "axlit", "axextend");

		-- Every base domains should depend on lang.
		for id in fileMap."base" repeat {
			addDependencies!(g, fileMap."lang", id);
		}

		-- Every other domain should depend on the aldor base domains.
		ids: List S := [];
		ids := append!(ids, copy fileMap."initaxiom");
		ids := append!(ids, copy fileMap."axiom");
		ids := append!(ids, copy fileMap."aldorext");
		for id in ids repeat addDependencies!(g, fileMap."base", id);

		-- "T" depends on "init_T"
		for id in fileMap."initaxiom" repeat {
			addDependency!(g, id, substring(id, 5));
		}
		
		-- Now there are two kinds of types:
		-- lower types and upper types.
		-- A type is a lower type if it is a dependency of
		-- axlit.as or axextend.as. That naturally includes all
		-- the elements of fileMap."base".
		-- A type is an upper type if it is not a lower type.
		-- In the terminology of this program, a type corresponds
		-- to a filename. So also axlit and lang are considered
		-- to be types.
		
		-- fileMap."aldorext" contains axlit and axextend.
		depTypes: Set S := [generator fileMap."aldorext"];
		lowerTypes: List S := sort! [
		    ancestors(g, generator depTypes)
		];
		TRACE("lowerTypes: ", lowerTypes);

		-- The types that axextend.as and axlit.as do not
		-- (recursively) depend on. So we make them depend
		-- on axextend and axlit.
		upperTypes: List S := [
		    s for s in allTypes | 
		      not member?(s, lowerTypes) and
		      not member?(s, fileMap."lang") and
		      not member?(s, fileMap."base") and
		      not member?(s, fileMap."initaxiom") and
		      not member?(s, fileMap."aldorext")
		];

		TRACE("upperTypes: ", upperTypes);
		for id in upperTypes repeat {
			addDependencies!(g, fileMap."aldorext", id);
		}
		TRACE("now all upper types depend on aldorext", "");
		cliques: Cliques := [g];
		TRACE("ConnectedComponents: ", cliques);

		---------------------------------------------------
		-- write out the components
		---------------------------------------------------
		-- Output all clique names.
		import from Clique, List Clique;
		out << "CLIQUES = \" << nl;
		for c in cliques repeat out << "  " << name c << "\" << nl;
		out << nl;

		-- Output all members and dependencies
		-- To find out about cliques with size bigger than 1 use:
		-- grep '^MEMBERS_.* = .* .*' cliques.mk
		initaxiom := fileMap."initaxiom";
		for c in cliques repeat writeVariables(c, initaxiom);
		for c in cliques repeat writeTargets c;
	}

	local writeVariables(clq: Clique, initaxiom: List S): () == {
		name: S := name clq;
		---------------------------------------------------
		-- write out the members
		---------------------------------------------------
		out << "MEMBERS__" << name << " =";

		-- We sort the members of the clq members, because if a
		-- domain is extended, it should come before anything
		-- else in the corresponding .ap file.

		-- x is smaller than y iff
		--   (1) x is "init_X" and y is not "init_Y"
		--   (2) x is "init_X" and y is "init_Y"
		--         and x is lex smaller than y
		--   (3) x is not "init_X" and y is not "init_Y"
		--         and "init_X" is in fileMap."initaxiom"
		--         and ("init_Y is not in file.Map."initaxiom"
		--             or x is lex smaller than y)
		--   (4) x is lex smaller than y
		local smaller?(x: S, y: S): Boolean == {
			local init?(id: S): Boolean == {
				import from MachineInteger;
				#id > 5 and substring(id, 5) = "init__";
			}
			init? x => {
				not init? y => true;
				x < y;
			}
			init? y => false;
			member?("init__" + x, initaxiom) => {
				member?("init__" + y, initaxiom) => x < y;
				true;
			}
			member?("init__" + y, initaxiom) => false;
			x < y;
		}
		sortedMembers: List S := sort!([id for id in clq], smaller?);
		for id in sortedMembers repeat out << " " << id;
		out << nl;

		---------------------------------------------------
		-- Write out all (recursive) dependencies.
		---------------------------------------------------
		out << "DEPS__" << name << " =";
		import from List Clique;
		for d in dependencies clq repeat out << " " << name d << ".ao";
		out << nl;
	}

	local writeTargets(clq: Clique): () == {
		import from List Clique;
		name: S := name clq;
		libaxiom := "al/libaxiom.al";
		libname  := "al/libaxiom__" + name + ".al";
		depmember:= "($(DEPS__" + name + "))";

		-- Each member depends on the corresponding .ao file.
		empty? dependencies clq => {
			out << libname << ":" << nl;
			out << tab << "ar cr $@" << nl;
		}
		-- Now we know that there is at least one dependency.
		-- What goes into the temporary library...
		out << libname << ": " << libname << depmember << nl;
		-- The order of members in the temporary library is
		-- as in libaxiom.al.
		out << libname << "(%.ao): ao/%.ao ";
		out << libaxiom << "(%.ao)" << nl;
		out << tab << "ar r $@ $<" << nl;
		-- A libaxiom.al member depends on DEPS_...
		out << libaxiom << "(" << name << ".ao): ";
		out << libaxiom << depmember << nl;
	}

	---------------------------------------------------
	-- Auxiliary functions follow.
	---------------------------------------------------

	-- Return the union of all recursive ancestors of 'types'.
	-- This function is only used to figure out the lower types.
	local ancestors(g: G, types: Generator S): Generator S == {
		ancs: Set S := [];
		for id in types repeat {
			ancs := insert!(id, union!(ancs, ancestors(g, id)));
		}
		generator ancs;
	}

	-- idx depends on dep
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
	generateMakefile(args.0);
	TRACE("END clq: ", args);
}

main();



