#!/usr/bin/env python3
'''r/dailyprogrammer Challenge #361 [Easy]

Description
=========================

5 Friends (let's call them a, b, c, d and e) are playing a game and need to
keep track of the scores. Each time someone scores a point, the letter of his
name is typed in lowercase. If someone loses a point, the letter of his name
is typed in uppercase. Give the resulting score from highest to lowest.

Input Description
=========================

A series of characters indicating who scored a point. Examples::

    abcde
    dbbaCEDbdAacCEAadcB

Output Description
=========================

The score of every player, sorted from highest to lowest. Examples::

    a:1, b:1, c:1, d:1, e:1
    b:2, d:2, a:1, c:0, e:-2

Challenge Input
=========================
::

    EbAAdbBEaBaaBBdAccbeebaec

Credit
=========================

This challenge was suggested by user /u/TheMsDosNerd, many thanks! If you have
any challenge ideas, please share them in /r/dailyprogrammer_ideas and there's
a good chance we'll use them.

https://www.reddit.com/r/dailyprogrammer/comments/8jcffg/20180514_challenge_361_easy_tally_program/
'''

def tally(input_str):
    '''Computes the score for each player.
    '''
    scores = {}
    for char in input_str:
        player = char.lower()
        old = scores.get(player, 0)
        change = 1 if char.islower() else -1
        scores[player] = old + change
    return scores


def print_scores(scores):
    '''Prints the scores in the format specified in the challenge.
    '''
    by_score = lambda x: x[1]
    scores = sorted(scores.items(), key=by_score, reverse=True)
    scores = (f'{player}:{score}' for player, score in scores)
    print(*scores, sep=', ')


if __name__ == '__main__':
    input_str = input('input scores: ')
    scores = tally(input_str)
    print_scores(scores)
