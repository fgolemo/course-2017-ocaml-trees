type e' redblacktree =
| Black of 'e redblacktree * 'e * 'e redblacktree
| Red of 'e redblacktree * 'e * 'e redblacktree
| Nil
