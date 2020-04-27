--Atschuthan Paskaran (500898502)
--Samir Muhammad (500896510)

module Poker where 
    import Data.List
    import Control.Monad
    import Control.Monad.Reader

    --this is where we output the winning hands
    deal hand = do 
        let evenhand = [hand!!0, hand!!2, hand!!4, hand!!6, hand!!8]
        let oddhand  = [hand!!1, hand!!3, hand!!5, hand!!7, hand!!9]
        let sortedoddhand = sort oddhand
        let sortedevenhand = sort evenhand
        let remainder1 = map (`mod` 13) sortedevenhand
        let sortedremainder1 = sort remainder1
        let remainder2 = map (`mod` 13) sortedoddhand
        let sortedremainder2 = sort remainder2
        let add13tokingpp1 = map (\x -> if (x==0) then 13 else x) sortedremainder1
        let add13tokingpp2 = map (\x -> if (x==0) then 13 else x) sortedremainder2
        let add13tokingp1 = sort add13tokingpp1
        let add13tokingp2 = sort add13tokingpp2
        let sump1 = pair add13tokingp1 + doublePair add13tokingp1 + threeOfAKind add13tokingp1 + straight add13tokingp1 + flush sortedevenhand + fullHouse add13tokingp1 + fourOfAKind add13tokingp1 + straightFlush add13tokingp1 + royalflush sortedevenhand
        let sump2 =  pair add13tokingp2 + doublePair add13tokingp2 + threeOfAKind add13tokingp2 + straight add13tokingp2 + flush sortedoddhand + fullHouse add13tokingp2 + fourOfAKind add13tokingp2 + straightFlush add13tokingp2 + royalflush sortedoddhand
        
        if sump1>sump2 then outputPlayer1 hand
        else if sump1<sump2 then outputPlayer2 hand
        else 
            if sump1 == 0 then highCardTie hand
            else if sump1 >= 100000000 && sump1<1000000000 then straightTie hand
            else if sump1<100000000 && sump1>=10000000 then straightTie hand
            else if sump1<10000000 && sump1>=1000000 then fourOfAKindTie hand
            else if sump1<1000000 && sump1>=100000 then fullHouseTie hand
            else if sump1<100000 && sump1>=10000 then flushTie hand
            else if sump1<10000 && sump1>=1000 then straightTie hand
            else if sump1<1000 && sump1>=100 then threeOfAKindTie hand
            else if sump1<100 && sump1>=10 then doublePairTie hand
            else if sump1<10 && sump1>=1 then pairTie hand
            else highCardTie hand
    
    --check if hand is a royalflush
    royalflush player = do
        if elem 1 player && elem 10 player && elem 11 player && elem 12 player && elem 13 player ||
            elem 14 player && elem 23 player && elem 24 player && elem 25 player && elem 26 player ||
            elem 27 player && elem 36 player && elem 37 player && elem 38 player && elem 39 player ||
            elem 40 player && elem 49 player && elem 50 player && elem 51 player && elem 52 player then 100000000
        else 0

    --check if hand is a pair 
    pair remainder = do
        let pair1 = length (filter(==remainder!!0) remainder)
        let pair2 = length (filter(==remainder!!1) remainder)
        let pair3 = length (filter(==remainder!!2) remainder)
        let pair4 = length (filter(==remainder!!3) remainder)
        let pair5 = length (filter(==remainder!!4) remainder)
        if pair1==2  
            || pair2==2  || pair3==2  || pair4==2 || pair5==2 then 1
        else 0

    --check if hand is a doublepair
    doublePair remainder = do
        let pair1 = length (filter (==remainder!!1) remainder)
        let pair2 = length (filter (==remainder!!2) remainder)
        let pair3 = length (filter (==remainder!!3) remainder)
        let pair4 = length (filter (==remainder!!pair1+pair2+pair3+1) remainder)
        if pair1==2 && pair2==2 || pair1==2 && pair3==2 || pair1==2 && pair4==2 || pair2==2 && pair3==2 || pair2==2 && pair4==2 || pair3==2 && pair4==4 then 10
        else 0

    --check if hand is a three of a kind
    threeOfAKind remainder = do
        let pair1 = length (filter (==remainder!!2) remainder)
        if pair1==3 then 100
        else 0

    --check if hand is a fullhouse
    fullHouse remainder = do
        let pair1 = length (filter(==remainder!!1) remainder)
        let pair2 = length (filter(==remainder!!1) remainder)
        let pair3 = length (filter(==remainder!!3) remainder)
        if pair1==3 && pair2==2 || pair1==3 && pair3==2 || pair1==2 && pair2==3 || pair2==3 && pair3==2 then 100000
        else 0

    --check is hand is a four of a kind
    fourOfAKind remainder = do
        let pair1 = length (filter(==remainder!!2) remainder)
        if pair1==4  then 1000000
        else 0
    
    --check if hand is a flush
    flush player = do
        let suitClubs = length(filter(<=13) player)
        let suitDiamonds = length(filter(liftM2 (&&) (>13)(<=26)) player)
        let suitHearts = length(filter(liftM2 (&&) (>26)(<=39)) player)
        let suitSpades = length(filter(liftM2 (&&) (>39)(<=52)) player)
        if suitClubs==5 || suitDiamonds==5 || suitHearts==5 || suitSpades==5 then 10000
        else 0
    
    --check if hand is a straight
    straight remainder = do 
        let rmdup = map head . group . sort
        let size = length(rmdup remainder)
        let first = remainder!!0
        let second = remainder!!1
        let last = remainder!!4
        if size==5 && second==(first+1) && (first+4)==last then 1000
        else 0
    
    --check if hand is a straightflush
    straightFlush player = do
        let first = player!!0
        let second = player!!1
        let third = player!!2
        let fourth = player!!3
        let last = player!!4
        let playersum = sum player
        if ((((first<13 && second<13 && third<13 && fourth<13 && last<=13 && second==first+1) || (first>13 && first<26 && second>13 && second<26 && third>13 && third<26 && fourth>13 && fourth<26 && last>13 && last<=26 && second==first+1)) || (first>26 && first<39 && second>26 && second<39 && third>26 && third<39 && fourth>26 && fourth<39 && last>26 && last<=39 && second==first+1)) || (first>39 && first<52 && second>39 && second<52 && third>39 && third<52 && fourth>39 && fourth<52 && last>39 && last<=52 && second==first+1)) then 10000000
        else 0

    --check which highcard wins
    highCardTie hand = do
        let evenhand = [hand!!0, hand!!2, hand!!4, hand!!6, hand!!8]
        let oddhand  = [hand!!1, hand!!3, hand!!5, hand!!7, hand!!9]
        let sortedoddhand = sort oddhand
        let sortedevenhand = sort evenhand
        let remainder1 = map (`mod` 13) sortedevenhand
        let sortedremainder1 = sort remainder1
        let remainder2 = map (`mod` 13) sortedoddhand
        let sortedremainder2 = sort remainder2
        let add13tokingp1 = map (\x -> if (x==0) then 13 else x) sortedremainder1
        let add13tokingp2 = map (\x -> if (x==0) then 13 else x) sortedremainder2
        if add13tokingp1!!4>add13tokingp2!!4 then outputPlayer1 hand
        else if add13tokingp1!!4<add13tokingp2!!4 then outputPlayer2 hand
        else
            if add13tokingp1!!3>add13tokingp2!!3 then outputPlayer1 hand
            else if add13tokingp1!!3<add13tokingp2!!3 then outputPlayer2 hand
            else
                if add13tokingp1!!2>add13tokingp2!!2 then outputPlayer1 hand
                else if add13tokingp1!!2<add13tokingp2!!2 then outputPlayer2 hand
                else
                    if add13tokingp1!!1>add13tokingp2!!1 then outputPlayer1 hand
                    else if add13tokingp1!!1<add13tokingp2!!1 then outputPlayer2 hand
                    else
                        if add13tokingp1!!0>add13tokingp2!!0 then outputPlayer1 hand
                        else if add13tokingp1!!0<add13tokingp2!!0 then outputPlayer2 hand
                        else
                            if sortedevenhand!!4>sortedoddhand!!4 then outputPlayer1 hand
                            else
                                outputPlayer2 hand 
    
    --check which three of a kind wins
    threeOfAKindTie hand = do
        let evenhand = [hand!!0, hand!!2, hand!!4, hand!!6, hand!!8]
        let oddhand  = [hand!!1, hand!!3, hand!!5, hand!!7, hand!!9]

        let sortedoddhand = sort oddhand
        let sortedevenhand = sort evenhand
        let remainder1 = map (`mod` 13) sortedevenhand
        let sortedremainder1 = sort remainder1
        let remainder2 = map (`mod` 13) sortedoddhand
        let sortedremainder2 = sort remainder2
        if sortedremainder1!!2 == 1 then outputPlayer1 hand
        else if sortedremainder2!!2 == 1 then outputPlayer2 hand
            else
                if sortedremainder1!!2 == 0 then outputPlayer1 hand
                else if sortedremainder2!!2 == 0 then outputPlayer2 hand
                else
                    if sortedremainder1!!2 > sortedremainder2!!2 then outputPlayer1 hand
                    else outputPlayer2 hand

    --check which four of a kind wins
    fourOfAKindTie hand = do
        let evenhand = [hand!!0, hand!!2, hand!!4, hand!!6, hand!!8]
        let oddhand  = [hand!!1, hand!!3, hand!!5, hand!!7, hand!!9]
        let sortedoddhand = sort oddhand
        let sortedevenhand = sort evenhand
        let remainder1 = map (`mod` 13) sortedevenhand
        let sortedremainder1 = sort remainder1
        let remainder2 = map (`mod` 13) sortedoddhand
        let sortedremainder2 = sort remainder2
        if sortedremainder1!!2 == 1 || sortedremainder1!!2 == 1 || sortedremainder1!!2 == 1 || sortedremainder1!!2 == 1 then outputPlayer1 hand
        else if sortedremainder2!!2 == 1 || sortedremainder2!!2 == 1 || sortedremainder2!!2 == 1 || sortedremainder2!!2 == 1 then outputPlayer2 hand
        else        
            if sortedremainder1!!2>sortedremainder2!!2  then outputPlayer1 hand
            else outputPlayer2 hand

    --check which fullhouse wins
    fullHouseTie hand = do
        let evenhand = [hand!!0, hand!!2, hand!!4, hand!!6, hand!!8]
        let oddhand  = [hand!!1, hand!!3, hand!!5, hand!!7, hand!!9]
        let sortedoddhand = sort oddhand
        let sortedevenhand = sort evenhand
        let remainder1 = map (`mod` 13) sortedevenhand
        let sortedremainder1 = sort remainder1
        let remainder2 = map (`mod` 13) sortedoddhand
        let sortedremainder2 = sort remainder2
        if sortedremainder1!!2 == 1 then outputPlayer1 hand
        else if sortedremainder2!!2 == 1 then outputPlayer2 hand
        else
            if sortedremainder1!!2>sortedremainder2!!2 then outputPlayer1 hand
            else outputPlayer2 hand

    --check which straight wins
    straightTie hand = do
        let evenhand = [hand!!0, hand!!2, hand!!4, hand!!6, hand!!8]
        let oddhand  = [hand!!1, hand!!3, hand!!5, hand!!7, hand!!9]
        let sortedoddhand = sort oddhand
        let sortedevenhand = sort evenhand
        let remainder1 = map (`mod` 13) sortedevenhand
        let sortedremainder1 = sort remainder1
        let remainder2 = map (`mod` 13) sortedoddhand
        let sortedremainder2 = sort remainder2
        if sortedremainder1!!4>sortedremainder2!!4 then outputPlayer1 hand
        else if sortedremainder2!!4>sortedremainder1!!4 then outputPlayer2 hand
        else if sortedevenhand!!4>sortedoddhand!!4 then outputPlayer1 hand
        else if sortedevenhand!!4<sortedoddhand!!4 then outputPlayer2 hand
        else outputPlayer2 hand

    --check which flush wins
    flushTie hand = do
        let evenhand = [hand!!0, hand!!2, hand!!4, hand!!6, hand!!8]
        let oddhand  = [hand!!1, hand!!3, hand!!5, hand!!7, hand!!9]
        let sortedoddhand = sort oddhand
        let sortedevenhand = sort evenhand
        let remainder1 = map (`mod` 13) sortedevenhand
        let sortedremainder1 = sort remainder1
        let remainder2 = map (`mod` 13) sortedoddhand
        let sortedremainder2 = sort remainder2
        if sortedremainder1!!4>sortedremainder2!!4 then outputPlayer1 hand
        else if sortedremainder2!!4>sortedremainder1!!4 then outputPlayer2 hand
        else if sortedremainder1!!3>sortedremainder2!!3 then outputPlayer1 hand
        else if sortedremainder2!!3>sortedremainder1!!3 then outputPlayer2 hand
        else if sortedremainder1!!2>sortedremainder2!!2 then outputPlayer1 hand
        else if sortedremainder2!!2>sortedremainder1!!2 then outputPlayer2 hand
        else if sortedremainder1!!1>sortedremainder2!!1 then outputPlayer1 hand
        else if sortedremainder2!!1>sortedremainder1!!1 then outputPlayer2 hand
        else if sortedremainder1!!0>sortedremainder2!!0 then outputPlayer1 hand
        else if sortedremainder2!!0>sortedremainder1!!0 then outputPlayer2 hand
        
        else if sortedevenhand!!4>sortedoddhand!!4 then outputPlayer1 hand
        else if sortedevenhand!!4<sortedoddhand!!4 then outputPlayer2 hand
        else if sortedevenhand!!3>sortedoddhand!!3 then outputPlayer1 hand
        else if sortedevenhand!!3<sortedoddhand!!3 then outputPlayer2 hand
        else if sortedevenhand!!2>sortedoddhand!!2 then outputPlayer1 hand
        else if sortedevenhand!!2<sortedoddhand!!2 then outputPlayer2 hand
        else if sortedevenhand!!1>sortedoddhand!!1 then outputPlayer1 hand
        else if sortedevenhand!!1<sortedoddhand!!1 then outputPlayer2 hand
        else if sortedevenhand!!0>sortedoddhand!!0 then outputPlayer1 hand
        else if sortedevenhand!!0<sortedoddhand!!0 then outputPlayer2 hand
        else outputPlayer1 hand

    --check which doublepair wins
    doublePairTie hand = do
        let evenhand = [hand!!0, hand!!2, hand!!4, hand!!6, hand!!8]
        let oddhand  = [hand!!1, hand!!3, hand!!5, hand!!7, hand!!9]
        let sortedoddhand = sort oddhand
        let sortedevenhand = sort evenhand
        let remainder1 = map (`mod` 13) sortedevenhand
        let sortedremainder1 = sort remainder1
        let remainder2 = map (`mod` 13) sortedoddhand
        let sortedremainder2 = sort remainder2
        let rmdup = map head . group . sort
        let first = rmdup remainder1!!0
        let second = rmdup remainder1!!1
        let third = rmdup remainder1!!2
        let first2 = rmdup remainder2!!0
        let second2 = rmdup remainder2!!1
        let third2 = rmdup remainder2!!1
        if remainder1!!3>remainder2!!3 then outputPlayer1 hand
        else if remainder1!!3<remainder2!!3 then outputPlayer2 hand
        else if remainder1!!1>remainder2!!1 then outputPlayer1 hand
        else if remainder1!!1<remainder2!!1 then outputPlayer2 hand
        else if third>third2 then outputPlayer1 hand
        else if third<third2 then outputPlayer2 hand
        else if second>second2 then outputPlayer1 hand
        else if second<second2 then outputPlayer2 hand
        else if first>first2 then outputPlayer1 hand
        else if first<first2 then outputPlayer2 hand
        else if sortedevenhand!!4>sortedoddhand!!4 then outputPlayer1 hand
        else if sortedevenhand!!4<sortedoddhand!!4 then outputPlayer2 hand
        else if sortedevenhand!!3>sortedoddhand!!3 then outputPlayer1 hand
        else if sortedevenhand!!3<sortedoddhand!!3 then outputPlayer2 hand
        else if sortedevenhand!!2>sortedoddhand!!2 then outputPlayer1 hand
        else if sortedevenhand!!2<sortedoddhand!!2 then outputPlayer2 hand
        else if sortedevenhand!!1>sortedoddhand!!1 then outputPlayer1 hand
        else if sortedevenhand!!1<sortedoddhand!!1 then outputPlayer2 hand
        else if sortedevenhand!!0>sortedoddhand!!0 then outputPlayer1 hand
        else if sortedevenhand!!0<sortedoddhand!!0 then outputPlayer2 hand
        else outputPlayer2 hand 

    --check which pair wins
    pairTie hand = do
        let player1 = [hand!!0, hand!!2, hand!!4, hand!!6, hand!!8]
        let player2  = [hand!!1, hand!!3, hand!!5, hand!!7, hand!!9]

        let sortedplayer2 = sort player2
        let sortedplayer1 = sort player1
        let remainder1 = map (`mod` 13) sortedplayer1
        let sortedremainder1 = sort remainder1
        let remainder2 = map (`mod` 13) sortedplayer2
        let sortedremainder2 = sort remainder2
        let p1pair= pairValues sortedremainder1
        let p2pair= pairValues sortedremainder2
        if p1pair > p2pair then outputPlayer1 hand
        else if p2pair > p1pair then outputPlayer2 hand

        else if sortedremainder1!!4 > sortedremainder2!!4 then outputPlayer1 hand
        else if sortedremainder2!!4 > sortedremainder1!!4 then outputPlayer2 hand
        else if sortedremainder1!!3 > sortedremainder2!!3 then outputPlayer1 hand
        else if sortedremainder2!!3 > sortedremainder1!!3 then outputPlayer2 hand
        else if sortedremainder1!!2 > sortedremainder2!!2 then outputPlayer1 hand
        else if sortedremainder2!!2 > sortedremainder1!!2 then outputPlayer2 hand
        else if sortedremainder1!!1 > sortedremainder2!!1 then outputPlayer1 hand
        else if sortedremainder2!!1 > sortedremainder1!!1 then outputPlayer2 hand
        else if sortedremainder1!!0 > sortedremainder2!!0 then outputPlayer1 hand
        else if sortedremainder2!!0 > sortedremainder1!!0 then outputPlayer2 hand

        else if sortedplayer1!!4 > sortedplayer2!!4 then outputPlayer1 hand
        else if sortedplayer2!!4 > sortedplayer1!!4 then outputPlayer2 hand
        else if sortedplayer1!!3 > sortedplayer2!!3 then outputPlayer1 hand
        else if sortedplayer2!!3 > sortedplayer1!!3 then outputPlayer2 hand
        else if sortedplayer1!!2 > sortedplayer2!!2 then outputPlayer1 hand
        else if sortedplayer2!!2 > sortedplayer1!!2 then outputPlayer2 hand
        else if sortedplayer1!!1 > sortedplayer2!!1 then outputPlayer1 hand
        else if sortedplayer2!!1 > sortedplayer1!!1 then outputPlayer2 hand
        else if sortedplayer1!!0 > sortedplayer2!!0 then outputPlayer1 hand
        else if sortedplayer2!!0 > sortedplayer1!!0 then outputPlayer2 hand
        else outputPlayer1 hand

    --find values for pair
    pairValues remainder = do 
        let pair1 = length(filter(==remainder!!0) remainder)
        let pair2 = length(filter(==remainder!!1) remainder)
        let pair3 = length(filter(==remainder!!2) remainder)
        let pair4 = length(filter(==remainder!!3) remainder)
        if pair1==2 then remainder!!0
        else if pair2==2 then remainder!!1
        else if pair3==2 then remainder!!2
        else if pair4==2 then remainder!!3
        else 0

    --adds suit to the winning hand 
    concatenation variable = do
        if variable>=1 && variable<=13 then show variable++("C")
        else if variable>=14 && variable<=26 then show (variable-13)++("D")
        else if variable>=27 && variable<=39 then show (variable-26)++("H")
        else if variable>=40 && variable<=52 then show (variable-39)++("S")
        else
            show variable++("C")

    --used to output player1 hand
    outputPlayer1 card = do
        let player1 = [card!!0, card!!2, card!!4, card!!6, card!!8]
        let sortedPlayer1 = sort player1
        let v1p1 = player1!!0
        let v2p1 = player1!!1
        let v3p1 = player1!!2
        let v4p1 = player1!!3
        let v5p1 = player1!!4

        let concv1 = concatenation v1p1
        let concv2 = concatenation v2p1
        let concv3 = concatenation v3p1
        let concv4 = concatenation v4p1
        let concv5 = concatenation v5p1
        let outputp1 = [concv1,concv2,concv3,concv4,concv5]
        let sortedp1 = sort outputp1
        sortedp1

    --used to output player2 hand
    outputPlayer2 card = do
        let player2 = [card!!1, card!!3, card!!5, card!!7, card!!9]
        let sortedPlayer2 = sort player2
        let v1p2 = player2!!0
        let v2p2 = player2!!1
        let v3p2 = player2!!2
        let v4p2 = player2!!3
        let v5p2 = player2!!4

        let concv1 = concatenation v1p2
        let concv2 = concatenation v2p2
        let concv3 = concatenation v3p2
        let concv4 = concatenation v4p2
        let concv5 = concatenation v5p2
        let outputp2 = [concv1,concv2,concv3,concv4,concv5]
        let sortedp2 = sort outputp2
        sortedp2