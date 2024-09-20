import sys
from collections import defaultdict
import graphlib

class DAG:
    # I think you can make it with defaultdict(list) but this makes more sense to me
    def __init__(self):
        self.graph = {}
    
    # Node need to add every node otherwise it'll never work out and crash since nodes with no children would crash
    def add_node(self, node):
        if node not in self.graph:
            self.graph[node] = []

    # added extra redunacy so I don't have to add every node into the graph
    # n1 = node1, n2 = node2
    def add_edges(self, n1, n2):
        if n1 not in self.graph:
            self.add_node(n1)
        if n2 not in self.graph:
            self.add_node(n2)
        self.graph[n1].append(n2)
    
    def topSort(self):
        visited = [False] * self.v
        stack = []
        for i in range(self.v):
            if not visited[i]:
                self._dfs(i, visited, stack)
        # reverses the stack 
        return stack[::-1]

    def _dfs( self, v, visited, stack):
        visited[v] = True
        for i in self.graph[v]:
            if not visited[i]:
                self._dfs(i, visited, stack)
        stack.append(v)

    def has_cycle(self):
        visited = [False] * self.v # True = visited False = yet to be viisted
        stack = [False] * self.v
        for node in range(self.v):
            if not visited[node]:
                if self.__cycle_check_helper(node, visited, stack):
                    return True
        return False
    
    def __cycle_check_helper(self, start, visited, stack):
        visited[start] = True
        stack[start] = True
        for nearby in self.graph[start]:
            if not visited[nearby]:
                return True
            elif stack[nearby]:
                return True
        stack[start]  = False
        return False

def add_tasks(enumed, t, bruh):
    for i in range(0,len(t),2):
        bruh.add_edge(t[i+1], enumed[t[i]])

def find_keys(d, target_value):
    return [key for key, value in d.items() if value == target_value]

def ordered_tasks(t, enumed):
    final = []
    for i in range(len(t)):
        final.append(find_keys(enumed,t[i]))
    return print_correctly(final)

def print_correctly(lst):
    for i in lst:
        if i and i != [None]:
            print(i[0])

# For debugging in pycharm
if len(sys.argv) < 2:
    print("Please provide the input file name.")
    sys.exit(1)

input_file = sys.argv[1]
with open(input_file, 'r') as file:
    tasks = file.read()

# tasks = sys.stdin.read()
enumed = enum(tasks)
bruh = DAG(len(tasks.splitlines()))
add_tasks(enumed, tasks.splitlines(), bruh)
if bruh.has_cycle():
    print("cycle")
    exit()
ordered_tasks(bruh.topSort(), enumed)
