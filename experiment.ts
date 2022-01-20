import { Record, Set, List, Map } from 'immutable'
import type { RecordOf } from 'immutable'

/*
class Circuit<T> {
    private vertices: T[] = []
    private edges: [T, T, T][] = []
    // private eq: (x: T, y: T) => boolean

    public constructor(eq?: (x: T, y: T) => boolean) {
        this.eq = eq ?? ((x, y) => x == y)
    }

    public addVertex(t: T): void {
        this.vertices.push(t)
    }

    public addEdge(l: T, r: T, out: T): void {
        this.edges.push([l, r, out])
    }

    public edgesContaining(t: T): [T, T, T][] {
        return this.edges.filter(([l, r, out]) => 
            this.eq(l, t) || this.eq(r, t) || this.eq(out, t)
        )
    }
}

class Graph {
    private vertices: Set<number> = new Set()
    private neighbors: Map<number, Set<number>> = new Map()
    private edges: [number, number][]

    public addVertex(vertex: number): void {
        this.vertices.add(vertex)
    }

    public hasEdge(u: number, v: number): boolean {
        return this.getNeighbors(u).has(v)
    }

    public addEdge(u: number, v: number): void {
        if (this.hasEdge(u, v)) {
            return
        }

        this.vertices.add(u)
        this.vertices.add(v)

        this.edges.push([u, v])

        if (this.neighbors.has(u)) {
            this.neighbors.get(u)?.add(v)
        } else {
            this.neighbors.set(u, new Set([v]))
        }

        if (this.neighbors.has(v)) {
            this.neighbors.get(v)?.add(u)
        } else {
            this.neighbors.set(v, new Set([u]))
        }
    }

    public getNeighbors(vertex: number): Set<number> {
        return this.neighbors.get(vertex) as Set<number>
    }
}
*/

/*
    Testing if immutable.js can correctly implement sets of objects.
*/

type IDCardProps = {
    name: string,
    age: number
}

const makeIDCard: Record.Factory<IDCardProps> = Record({name: 'Bob', age: 69})

type IDCard = RecordOf<IDCardProps>

const jason = makeIDCard({name: 'Jason', age: 77})
const matthew = makeIDCard({name: 'Matthew', age: 100})
const glass = makeIDCard({name: 'Glass', age: 23})

const jason2 = makeIDCard({name: 'Jason', age: 77})

const mySet: Set<IDCard> = Set()

const withoutJason2 = mySet.add(jason).add(matthew).add(glass)

const withJason2 = withoutJason2.add(jason2)


// console.log(withJason2.equals(withoutJason2)) => true


/*
    What about without records?
*/

type BillGates = {name: string, creditScore: number}

const phillip: BillGates = {name: 'Phillip', creditScore: 100}

const seymour: BillGates = {name: 'Seymour', creditScore: 300}

const hoffman: BillGates = {name: 'Hoffman', creditScore: 150}

const phillip2: BillGates = {name: 'Phillip', creditScore: 100}

const withoutPhillip2 = Set()
    .add(phillip)
    .add(seymour)
    .add(hoffman)

const withPhillip2 = withoutPhillip2.add(phillip2)

// console.log(withoutPhillip2.equals(withPhillip2)) => false

type VertexProps = {
    id: number,
    x: number,
    y: number
}

const makeVertex = Record<VertexProps>({id: 0, x: 0, y: 0})

type Vertex = RecordOf<VertexProps>

type EdgeProps = {
    u: Vertex,
    v: Vertex
}

const makeEdge2 = Record<EdgeProps>({u: makeVertex(), v: makeVertex()})

function minMax(u: Vertex, v: Vertex): [Vertex, Vertex] {
    if (u.id < v.id) {
        return [u, v]
    } else {
        return [v, u]
    }
}

function makeEdge({u, v}: {u: Vertex, v: Vertex}): Edge {
    const [u1, v1] = minMax(u, v)
    return makeEdge2({u: u1, v: v1})
}

type Edge = RecordOf<EdgeProps>

type GraphProps = {
    vertices: Set<Vertex>
    edges: Set<Edge>
}

const makeGraph = Record<GraphProps>({
    vertices: Set(),
    edges: Set(),
})

type Graph = RecordOf<GraphProps>

type TickedGraphProps = {
    vertices: Set<Vertex>
    edges: Map<Edge, 'ticked' | 'unticked'>
}

const makeTickedGraph = Record<TickedGraphProps>({
    vertices: Set(),
    edges: Map(),
})

type TickedGraph = RecordOf<TickedGraphProps>

type GateProps = {
    in: Set<TickedGraph>
    out: TickedGraph
}

const makeGate = Record<GateProps>({in: Set(), out: makeTickedGraph()})

type Gate = RecordOf<GateProps>

type CircuitProps = {
    values: Set<TickedGraph>
    gates: Set<Gate>
}

const makeCircuit = Record<CircuitProps>({values: Set(), gates: Set()})

type OddCycleProps = {
    cycle: List<Vertex>
    graph: TickedGraph
}

const makeOddCycle = Record<OddCycleProps>({cycle: List(), graph: makeTickedGraph()})

type OddCycle = RecordOf<OddCycleProps>

function addEdge(graph: Graph, u: Vertex, v: Vertex): Graph {
    return makeGraph({
        vertices: graph.vertices.add(u).add(v),
        edges: graph.edges.add(makeEdge({u, v}))
    })
}

function addUntickedEdge(graph: TickedGraph, u: Vertex, v: Vertex): TickedGraph {
    return makeTickedGraph({
        vertices: graph.vertices.add(u).add(v),
        edges: graph.edges.set(makeEdge({u, v}), 'unticked')
    })
}

function fromEdgeList(edges: [number, number][]): Graph {
    let g = makeGraph()

    for (let [u, v] of edges) {
        g = addEdge(g, makeVertex({id: u}), makeVertex({id: v}))
    }

    return g
}

function allOddCycles(g: Graph): Set<List<Vertex>> {
    const neighbors = calculateNeighbors(g)

    return g.vertices.flatMap(vertex =>
        allOddCyclesHelper(
            g, vertex, vertex,
            List([vertex]),
            Set([vertex]),
            neighbors
        )
    )
}

function allOddCyclesHelper(
    g: Graph,
    startingVertex: Vertex,
    currentVertex: Vertex,
    path: List<Vertex>,
    seen: Set<Vertex>,
    neighbors: Map<Vertex, Set<Vertex>>
): Set<List<Vertex>> {
    const myNeighbors = neighbors.get(currentVertex)

    if (myNeighbors === undefined) {
        throw new Error(`invalid vertex: ${currentVertex.id} ${g}`);
    }

    const result = myNeighbors
        .filterNot(neighbor => seen.contains(neighbor))
        .flatMap(neighbor => {
            return allOddCyclesHelper(
                g, startingVertex, neighbor,
                path.push(neighbor),
                seen.add(neighbor),
                neighbors
            )
        })

    if (
        (myNeighbors.includes(startingVertex)) &&
        (path.size >= 3) &&
        (path.size % 2 === 1)
    ) {
        return result.add(path)
    } else {
        return result
    }
}

function convertOddCycle(vertices: List<Vertex>): TickedGraph {
    const vs = vertices.toArray()

    let g = makeTickedGraph()

    for (let i = 0; i < vs.length - 1; i++) {
        g = addUntickedEdge(g, vs[i], vs[i + 1])
    }

    g = addUntickedEdge(g, vs[0], vs[vs.length - 1])

    return g
}

const diamond = fromEdgeList([
    [0, 1],
    [0, 2],
    [1, 2],
    [1, 3],
    [2, 3]
])

const cycles = allOddCycles(diamond)

const oddCycleMap: Map<Set<Vertex>, List<Vertex>> = Map(cycles.map(x => [x.toSet(), x]))

for (let cycle of oddCycleMap.values()) {
    // console.log(cycle.map(x => x.id).toArray())
}

function hasEdge(g: Graph, u: Vertex, v: Vertex): boolean {
    return g.edges.has(makeEdge({u, v}))
}

function hasTickedEdge(g: TickedGraph, vertex: Vertex): boolean {
    for (let [edge, kind] of g.edges) {
        if (
            (edge.u.equals(vertex) || edge.v.equals(vertex)) &&
            (kind === 'ticked')
        ) {
            return true
        }
    }

    return false
}

function allSubsets<T>(set: Set<T>): Set<Set<T>> {
    if (set.isEmpty()) {
        return Set([Set()])
    } else {
        const first = set.first() as T
        const rest = set.rest()

        return allSubsets(rest).flatMap(x => Set([
            x.add(first),
            x
        ]))
    }
}

function crossProduct<A, B, C>(xs: Set<A>, ys: Set<B>, f: (a: A, b: B) => C): Set<C> {
    let tmp: C[] = []

    for (let x of xs) {
        for (let y of ys) {
            tmp.push(f(x, y))
        }
    }

    return Set(tmp)
}

function union(a: TickedGraph, b: TickedGraph): TickedGraph {
    return makeTickedGraph({
        vertices: a.vertices.union(b.vertices),
        edges: mergeEdges(a.edges, b.edges)
    })
}

// Ticked edges take priority. Note that there should never be a case where both edges are ticked. (Or, rather, if there ARE cases, then you can employ the rearrangement lemma to remove those cases.)
function mergeEdges(e1: Map<Edge, 'ticked' | 'unticked'>, e2: Map<Edge, 'ticked' | 'unticked'>): Map<Edge, 'ticked' | 'unticked'> {
    return e1.mergeWith(
        (x, y) => x === 'ticked' || y === 'ticked' ? 'ticked' : 'unticked',
    e2)
}

function convert(g: Graph): TickedGraph {
    return makeTickedGraph({
        vertices: g.vertices,
        edges: g.edges.toMap().map(_ => 'unticked')
    })
}

function addAllTickedEdges(g: TickedGraph, us: Set<Vertex>, vs: Set<Vertex>): TickedGraph {
    let tmp: Edge[] = []

    for (let u of us) {
        for (let v of vs) {
            tmp.push(makeEdge({u, v}))
        }
    }

    const edges = Set(tmp)

    return makeTickedGraph({
        vertices: g.vertices,
        edges: mergeEdges(g.edges, edges.toMap().map(_ => 'ticked'))
    })
}

function calculateNeighbors(g: Graph): Map<Vertex, Set<Vertex>> {
    let m = Map<Vertex, Set<Vertex>>()

    for (let edge of g.edges) {
        m = m.update(edge.u, Set(), x => x.add(edge.v))
        m = m.update(edge.v, Set(), x => x.add(edge.u))
    }

    return m
}

function tickedGraphToString(g: TickedGraph): string {
    const blueVertices = g.vertices
        .filter(x => hasTickedEdge(g, x))
        .map(x => x.id)
        .toArray()

    const whiteVertices = g.vertices
        .filterNot(x => hasTickedEdge(g, x))
        .map(x => x.id)
        .toArray()

    return [
        `blueVertices: ${blueVertices}`,
        `whiteVertices: ${whiteVertices}`
    ].join('\n')
}

function compute(g: Graph): Set<Gate> {
    const oddCycles = allOddCycles(g)
    const uniqueOddCycles = Map(oddCycles.map(x => [x.toSet(), x]))

    const baseCases: Set<TickedGraph> = Set(
        uniqueOddCycles.map(convertOddCycle).values()
    )

    let gates = Set<Gate>()

    let establishedGraphs = baseCases
    let newGraphs = baseCases
    let newGates = Set<Gate>()
    do {
        newGates = allXis(g, establishedGraphs, newGraphs)
        gates = gates.union(newGates)
        newGraphs = newGates.map(gate => gate.out)
        establishedGraphs = establishedGraphs.union(newGraphs)
    } while (newGates.size !== 0)

    return gates
}

function allXis(g: Graph, establishedGraphs: Set<TickedGraph>, newGraphs: Set<TickedGraph>): Set<Gate> {
    let gates: Gate[] = []

    for (let a of establishedGraphs) {
        for (let b of newGraphs) {
            const merged = union(a, b)

            const aCandidates = a.vertices.filterNot(u => hasTickedEdge(a, u))
            const bCandidates = b.vertices.filterNot(v => hasTickedEdge(b, v))

            const aBreakdowns = breakdowns(aCandidates)
            const bBreakdowns = breakdowns(bCandidates)

            for (let [o1, i1] of aBreakdowns) {
                for (let [o2, i2] of bBreakdowns) {
                    if (xi(g, o1, i1, i2, o2)) {
                        gates.push(makeGate({
                            in: Set([a, b]),
                            out: addAllTickedEdges(merged, i1, i2)
                        }))
                    }
                }
            }
        }
    }

    return Set(gates)
}

function disjoint<T>(a: Set<T>, b: Set<T>): boolean {
    return a.intersect(b).isEmpty()
}

function xi(g: Graph, o1: Set<Vertex>, i1: Set<Vertex>, i2: Set<Vertex>, o2: Set<Vertex>) {
    return (
        (!i1.isEmpty()) &&
        (!i2.isEmpty()) &&
        (o1.size + o2.size !== 0) &&
        disjoint(i1, i2) &&
        disjoint(o1, i2) &&
        disjoint(o2, i1) &&
        hasCompleteBipartiteSubgraph(g, i1, i2)
    )
}

function hasCompleteBipartiteSubgraph(g: Graph, us: Set<Vertex>, vs: Set<Vertex>): boolean {
    for (let u of us) {
        for (let v of vs) {
            if (!hasEdge(g, u, v)) {
                return false
            }
        }
    }

    return true
}

function breakdowns<T>(s: Set<T>): List<[Set<T>, Set<T>]> {
    if (s.isEmpty()) {
        return List([[Set(), Set()]])
    }

    let tmp: [Set<T>, Set<T>][] = []
    const restBreakdowns = breakdowns(s.rest())

    for (let [a, b] of restBreakdowns) {
        tmp.push([a.add(s.first()), b               ])
        tmp.push([a,                b.add(s.first())])
    }

    return List(tmp)
}

// main

const tripleGraph = fromEdgeList([
    [0, 1],
    [0, 2],
    [1, 2],
    [2, 3],
    [3, 4],
    [3, 5],
    [4, 5],
    [5, 6],
    [6, 7],
    [6, 8],
    [7, 8]
])

const gates = compute(tripleGraph)

const [x, y] = gates.map(x => x.out).rest().rest()

console.log(`Vertices: ${x.vertices.equals(y.vertices)}`)
console.log(`Edges: ${x.edges.equals(y.edges)}`)
console.log(`Edges keys: ${Set(x.edges.keys()).equals(Set(y.edges.keys()))}`)

for (let key of x.edges.keys()) {
    if (x.edges.get(key) !== y.edges.get(key)) {
        console.log(`Discrepancy in edge ${key.u.id}, ${key.v.id}`)
    }
}

x.edges.merge(y.edges)

for (let graph of gates.map(x => x.out).rest().rest()) {
    console.log(tickedGraphToString(graph))
}
