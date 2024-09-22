# CS488 Independent Study: Compiler Design
This is a repo for my Independent Study with Professor Kellogg. We skipped over the parser and lexical parts due to them being covered in CS280.

# PA1 Overview
PA1 is a "simple" topological sort, mainly to test if you have the mettle to try this class. The first checkpoint should be easy if you've never seen it before, similar to graph theory with vertices and an edge list. Vertices represent a list of all nodes, which you use to start the path. The edge list tells you which paths to go through. You can either do a DFS or a BFS (Kahn's algorithm) to figure it out. BFS will be easier with programs that have map data structures, while DFS is harder to implement. However, if you go through with DFS, you won't have to change it even in COOL, which has no data structures and will force you to make EVERYTHING from scratch. Hint: to make it deterministic, make sure that you set the edge list to sort against each other.
Some resoucres:
https://www.geeksforgeeks.org/topological-sorting/ 
How you should view it:
https://assets.leetcode.com/users/images/63bd7ad6-403c-42f1-b8bb-2ea41e42af9a_1613794080.8115625.png