# for graphviz-plotting a normal binary search tree:
./3_12-graphtree.native | dot -Tpng -o tree.png && xdg-open tree.png

# after creating and compiling the function for graphviz-plotting a red-black tree:
./4_7-graphrbtree.native | dot -Tpng -o tree-rb.png && xdg-open tree-rb.png

# now same R/B tree, but with proper spacing:
./4_7-graphrbtree.native | dot | gvpr -c -ftree.gv | neato -n -Tpng -o tree-rb-spaced.png && xdg-open tree-rb-spaced.png
