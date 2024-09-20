import sys
import heapq

# High-level overview for part 2-4 (mostly for me):
'''
__init__ is for initializing graph is made of a dict which holds a list of all children or next tasks, num_nodes is for
number of nodes, and in_degree is for the amount of tasks that are a prerequisite of that task stored as a dictionary
with a count of how many dependent tasks

add_node is for a big problem I had where going through the graph not every node was included in the dict because it was
in the child list instead of the graph. It also updates in_degree and num_nodes accordingly.

connect_node adds the nodes into each others child list. It also updates the dependency task dictionary accordingly for
each task added

topSort is now a Khan's algo with a min heap for the alphabetical requirement. For the first node that do not require
any other prerequisites (in_degree 0) it adds them to the heap. Since its a min heap, the heap is sorted lowest to 
highest already (this was the problem that I could not figure out with DFS). When it enters the loop the smallest node
is popped and added to the final list. For the loop inside the while, since the while loop just processed a prerequisite,
it decrease the in_degree by 1. If any children now have an in_degree 0 it adds them to the heap. 

has_cycle is a left over from the DFS implementation however, it still worked when I switched to Khan's algo, so I'm not
gonna change it. This works by creating a dictionary for each node and traversing through the graph and marking each
child as visited. The helper function goes through the list marking each one as visited but if detects a path as already
visited a cycle has been detected and returns true. After traversing each child path it breaks out then continues on the 
next child path. The important list is stack, which is used to detect if a path has occurred. Visited is just for
marking a path. 
'''
class DAG:
    # I think you can make it with defaultdict(list) but this makes more sense to me
    # in_degree is needed for using Khan's otherwise DFS does not need it
    def __init__(self):
        self.graph = {}
        self.num_nodes = 0
        self.in_degree = {key: 0 for key in self.graph.keys()}

    # Node need to add every node otherwise it'll never work out and crash since nodes with no children would crash
    def add_node(self, node):
        if node not in self.graph:
            self.graph[node] = []
            self.num_nodes += 1
            self.in_degree[node] = 0

    # added extra redundancy, so I don't have to add every node into the graph
    # n1 = node1, n2 = node2
    # n1 points towards n2 thus becoming its restraint
    def connect_nodes(self, n1, n2):
        if n1 not in self.graph:
            self.add_node(n1)
        if n2 not in self.graph:
            self.add_node(n2)
        self.graph[n1].append(n2)
        self.in_degree[n2] += 1

    def topSort(self):
        # Leftover from DFS attempt, might need it later on
        # visited = {key: False for key in self.graph}
        # stack = []
        # for task in self.graph: # sort for the alphabetical requirement
        #     self.graph[task].sort()
        # print("Inside graph:", self.graph,self.num_nodes)
        # for i in self.graph:
        #     # print("At i:", i)
        #     # print("At graph[i]:", self.graph[i])
        #     if not visited[i]:
        #         self._dfs(i, visited, stack) # use helper
        # reverse the stack since recursive found path
        # return stack[::-1]
        # This is the khan's algo implementation of this, since DFS with some random sorting would not work
        min_heap = []
        for node in self.graph:
            if self.in_degree[node] == 0:
                heapq.heappush(min_heap, node)
        final = []
        while min_heap:
            curr = heapq.heappop(min_heap) # pop and add the root to the final list
            final.append(curr)
            for child in self.graph[curr]:
                self.in_degree[child] -= 1
                if self.in_degree[child] == 0:
                    heapq.heappush(min_heap, child) # no prerequisites left add to the heap which will add to the final
                    # at the top of the while loop
        return final

    # Tried dfs first did not work out well, I hate it
    # def _dfs(self, v, visited, stack):
    #     visited[v] = True
    #     for i in self.graph[v]:
    #         if not visited[i]:
    #             self._dfs(i, visited, stack)
    #     stack.append(v) # keeps track of the path

    def has_cycle(self):
        visited = {key: False for key in self.graph} # True = visited False = yet to be visited, used for tracing path
        stack = {key: False for key in self.graph} # used for checking if cycle
        for node in self.graph:
            if not visited[node]:
                if self.__cycle_check_helper(node, visited, stack):
                    return True
        return False

    def __cycle_check_helper(self, start, visited, stack):
        visited[start] = True
        stack[start] = True # mark for tracing path so you can know that you are looping
        for nearby in self.graph[start]:
            if not visited[nearby]:
                if self.__cycle_check_helper(nearby, visited, stack): # if not visited explore children
                    return True
            elif stack[nearby]: # if true node detected on stack that means the node is looping on itself
                return True
        stack[start] = False # recursion ended so trace path is not needed anymore, important to not trigger everything
        # as a cycle
        return False

    # for my own testing, to see if loaded correctly
    def print_keys(self):
        for i in self.graph:
            print(i)
        print("Number of nodes:", self.num_nodes)

# used to add tasks, remember first line is name and second line is the parent
def add_tasks(t, bruh):
    for i in range(0, len(t), 2):
        bruh.connect_nodes(t[i + 1], t[i])

# def find_keys(d, target_value):
#     return [key for key, value in d.items() if value == target_value]

# def ordered_tasks(t, enumed):
#     final = []
#     for i in range(len(t)):
#         final.append(find_keys(enumed, t[i]))
#     return print_correctly(final)
# has some weird problems when printing regularly so I made this function
def print_correctly(lst):
    for i in lst:
        if i and i != [None]:
            print(i)

# For debugging in pycharm
# if len(sys.argv) < 2:
#     print("Please provide the input file name.")
#     sys.exit(1)
#
# input_file = sys.argv[1]
# with open(input_file, 'r') as file:
#     tasks = file.read()

tasks = sys.stdin.read()
bruh = DAG()
add_tasks(tasks.splitlines(), bruh)
# bruh.print_keys()
if bruh.has_cycle():
    print("cycle")
    exit()
print_correctly(bruh.topSort())
