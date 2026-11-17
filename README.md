
# souk



A simulated village for studying how institutions emerge around trade, in R.

Players need one unit of every good each day. They choose what to make,
swap goods one for one, and invest their free hours in their skills. On top
of that you can switch on groups that trade among themselves first,
droughts, taxes that groups vote on, and an invention.

## Install

```r
# install.packages("pak")
pak::pak("bennour007/souk")
```

## Example

```r
library(souk)

village <- new_village(n_players = 100, n_goods = 8, seed = 3)
rules   <- set_rules(groups = list(on = TRUE))

run <- run_village(village, n_days = 300, rules = rules)

run$players   # one row per player per day
run$groups    # one row per group per day
```

## How it is built

- **Rules** come in sets: `world`, `skills`, `player`, `market`, `groups`,
  `tax`, `drought`, `invention`. Every rule has a default, and a typo is an
  error. See `?set_rules`.
- **A day** is a schedule of steps: events, membership, specialising, the
  market, learning, investing, voting. Each step takes the village and
  returns it, so you can replace one or add your own. See
  `?default_schedule`.
- **A run** records the village after every day and returns the start and
  end states.

## Limits

Goods are swapped one for one: there are no prices and no money. Players
judge their chances by the whole village and ignore their own group. Only
one invention can happen per run.

## Read more


I haven't finished writing [chapter 2](https://bennour.xyz/projects/souk/) properly yet, but you can see in there some extra details on how to use it

## License

MIT
