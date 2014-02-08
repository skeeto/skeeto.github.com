---
title: Chess AI Idea
layout: post
tags: [ai, game]
uuid: 2e66347a-fe5d-3b70-58ea-9a9e9bd6248a
---

So, I had this idea using a genetic algorithm to optimize the
parameters of a program that plays chess. Now, the genetic algorithm
wouldn't be used at all during a game, but rather to optimize the
board evaluation parameters beforehand. I don't know much about
writing board game AI programs, as I have only written a few of them
for fun (tic-tac-toe, connect 4, Pente). For this chess program, I am
taking a simple approach because I am more interested in seeing the
genetic algorithm at work than seeing the chess playing AI do well
against other chess AI or people.

The program would search the game tree using the [minimax][minimax]
algorithm, with some [possible optimizations][pruning] added
afterward. Tree searching is just a matter of generating all possible
moves and looking at them. The hard part is the board evaluation
function, which evaluates the a particular board's score based on the
arrangement of the pieces. Parameters to this evaluation function
would be, for example, the piece values. The pawn would be locked in
at a value of 1, which anchors the other values and provides a base
unit to work from.

Again, the parameters would not change during a game. We use the
genetic algorithm ahead of time to determine the parameters.

For the genetic algorithm, the set of parameters strung together makes
up a single chromosome. We maintain a pool of different chromosomes,
i.e. different sets of parameters, and breed these chromosomes
together to improve our parameter sets. We start out with a random
pool made of parameters that are most likely pretty terrible.

To evaluate the chromosomes, we need a fitness function, which
evaluates each chromosome for its level of "fitness" deciding if it
breeds or not. To do this we simply play the chromosome we are
evaluating against some base chromosome, which may just be parameters
chosen intuitively by the programmer. Or, the base chromosome could be
random too. Starting with a better base chromosome would be a good
head start, though. The fitness of the chromosome is how often it wins
against the base chromosome in, say, a few hundred games.

The most fit chromosomes are bred by taking a few parameters from each
to make a new chromosome. Mutations are occasionally added in order to
keep the chromosome pool from getting stuck in a local maximum. A
mutation involves changing one or more parameters in a chromosome
slightly in some random way. Mutations are rare and will usually be
detrimental to the chromosome quickly killing it off, but will
occasionally cause a good change that will be spread to other
chromosomes in the next generation, improving the gene pool.

We iterate this until either the maximum fitness level in the pool is
stuck for several iterations (we aren't getting anywhere and mutations
aren't helping), or the chromosomes are so good they always beat the
base chromosome, making the fitness algorithm meaningless. When this
happens, we replace the base chromosome with the best chromosome in
the pool and start over from scratch again with a random, or mostly
random, pool.

As you would expect, I have looked into parallelizing this process to
take advantage of a cluster. This is easy for several reasons. First,
evaluating chromosomes can be done simultaneously. No evaluation
depends on another chromosome's evaluation. Second, the minimax game
tree search can be parallelized so that several different processes
search the game tree and give their results back to the parent
process. This works very well because the data being sent back to the
parent will be a single integer. No need to send large amounts of data
around the network.

I spent an afternoon hacking at this, but its still too crude to share
yet. I got the non-parallel version of the chess engine built but I am
still working on the evaluation function. The genetic algorithm hasn't
been started. The only parameters at the moment are piece values. The
board evaluation function just adds up the piece values on the board
completely ignoring their positions. This makes the computer play
extremely aggressively, capturing the opponent's pieces whenever it
can. This makes for a somewhat interesting bloodbath where the board
goes empty after just a few moves.

My problem right now is finding a good way to represent piece
movements so that it can be recycled. That is, I want to represent
movements when generating my search tree, verifying the legality of a
move, and evaluating the board all the same way so that I don't have
to program in piece movements several times.


[minimax]: http://en.wikipedia.org/wiki/Minimax
[pruning]: http://en.wikipedia.org/wiki/Alpha-beta_pruning
