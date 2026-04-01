# Doomsday algorithm training program

This is a command line program to train **determining the day of the week for any date.**

The algorithm ([wiki](https://en.wikipedia.org/wiki/Doomsday_rule)) was devised by the mathematician
John Conway to be calculated mentally.

It's easy and uses remainders of 7 (because we only care about the weekday) to make big numbers small.

## Usage

You can build and run the `doomsday` executable with Cabal:

```sh
cabal run -- train
```
```
Which day of the week will be 5 April 2026?
> Su
Sunday is correct! The weekday will be Sunday.

Which day of the week was 17 January 2026?
> 1
Monday is wrong! The weekday was Saturday.

Which day of the week will be 25 May 2026?
> ?
Find the weekday. Starting with date 25 May 2026:
 - note the year anchor W = Saturday
 - this months doomsday is O = 9 (9-to-5)
 - the resulting increment is I = D - O = 25 - 9 = 16 ≡ Tuesday
 - add the year anchor to get R = W + I = Saturday + Tuesday = 8 ≡ Monday

The weekday will be Monday.

Which day of the week will be 24 December 2026?
> Ctrl+D

Average: 50%
Count correct/wrong/total: 2/1/4
Speed correct/wrong/average: 36.03s/19.48s/30.52s
```

## Plots

You can view how you are improving with plots made with [granite](https://github.com/mchav/granite):
```sh
cabal run doomsday -- plot bars
```
```
Elapsed time to correct answer
  77.2│                                                            
      │                                                            
      │                    ┬                                       
      │                    │                                       
      │                    │                                       
      │                    │                                       
      │                    │                                       
      │═══┬═══             │                                       
      │                    │                                       
      │                    │                                       
  38.8│                    │                                       
      │                 │─────│                                    
      │                 │     │                                    
      │                 │     │                                    
      │                 ═══════                                    
      │                 │     │                                    
      │                 │     │                                    
      │                 │─────│                                    
      │                    │                                       
   0.5│                    ┴             ═══┬═══          ═══┬═══  
      └────────────────────────────────────────────────────────────
     Same month       Other month      Other year     Other centuries
```
