import java.io.InputStreamReader
import java.io.BufferedReader
import java.util.PriorityQueue

class DAG {
    var graph: MutableMap<String, MutableList<String>> = mutableMapOf()
    var numNodes: Int = 0
    var inDegree: MutableMap<String, Int> = mutableMapOf()

    init {
        for (key in graph.keys) {
            inDegree[key] = 0
        }
    }

    fun add_node(node: String) {
        if (node !in graph) {
            graph[node] = ArrayList<String>()
            numNodes += 1
            inDegree[node] = 0
        }
    }

    fun connect_nodes(node1: String, node2: String) {
        if (node1 !in graph)
            add_node(node1)
        if (node2 !in graph)
            add_node(node2)
        graph[node1]?.add(node2)
        inDegree[node2] = inDegree[node2]!! + 1
    }

    fun topSort() : ArrayList<String> {
        var minHeap = PriorityQueue<String>()
        for ((node,_) in graph) {
            if (inDegree.getOrDefault(node, 1) == 0) {
                minHeap.add(node)
            }
        }
        var final = ArrayList<String>()
        while (!minHeap.isEmpty()) {
            var curr: String = minHeap.poll()
            final.add(curr)
            for (child in graph[curr]!!) {
                inDegree[child]?.minus(1)
                if (inDegree[child] == 0) {
                    minHeap.add(child)
                }
            }
        }
        return final
    }

    fun hasCycle() : Boolean {
        var visited: MutableMap<String, Boolean> = mutableMapOf()
        var stack: MutableMap<String, Boolean> = mutableMapOf()
        for (key in graph.keys){
            visited[key] = false
            stack[key] = false
        }
        for (node in graph.keys) {
            if (!visited[node]!!)
                if (cycleCheckHelper(node, visited, stack))
                    return true
        }
        return false
    }

    fun cycleCheckHelper(node: String, visited: MutableMap<String, Boolean>, stack: MutableMap<String, Boolean>) :Boolean {
        visited[node] = true
        stack[node] = true
        for (nearby in graph[node]!!) {
            if (!visited[nearby]!! ?: false) {
                if (cycleCheckHelper(node, visited, stack))
                    return true
            }
            else if (stack[nearby] ?: false)
                return true
        }
        stack[node] = false
        return false
    }
}

fun addTasks(t: ArrayList<String>, bruh: DAG) {
    for (i in 0 until t.size step 2){
        bruh.connect_nodes(t[i+1], t[i])
    }
}

fun main()
{
    val reader = BufferedReader(InputStreamReader(System.`in`))
    val tasks = ArrayList<String>()
    while(true){
        val line = reader.readLine() ?: break
        tasks.add(line)
    }
    val bruh = DAG()
    addTasks(tasks, bruh)
    println(bruh.hasCycle())
    println(bruh.topSort())
}
