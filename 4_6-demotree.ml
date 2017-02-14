type e' redblacktree =
| Black of 'e redblacktree * 'e * 'e redblacktree
| Red of 'e redblacktree * 'e * 'e redblacktree
| Nil

let tree_rb =
  Black(
    Red(
      Black(
        Red(Nil, 3, Nil),
        4,
        Red(Nil, 7, Nil)
        ),
      10,
      Black(Nil, 12, Nil)
      ),
    13,
    Red(
      Black(
        Nil,
        15,
        Red(Nil, 20, Nil)
        ),
      30,
      Black(
        Nil,
        35,
        Red(Nil, 40, Nil)
        )
      )
    )
