# BlackJack

Uses stochastic optimization to tell the player whether to hit or to stand, depending on their own cards and on the dealer's face-up card.

States are defined as follows: [number of high aces (==0 or 1)]A[total value of other cards] (except for "21" and "22", the latter which represents a bust)
