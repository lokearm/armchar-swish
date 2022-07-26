---
title: The Web API described in Tests
---


# GET (main calls)

To get a list of advancements.  Here, `cieran` is the ID of a character.

```sh
http get :3000/show/adv/cieran
```

Character sheets are retrieved one trait type at a time.

```sh
http get :3000/virtue/cieran/1217/Summer
http get :3000/flaw/cieran/1217/Summer
http get :3000/pt/cieran/1217/Summer
http get :3000/ability/cieran/1217/Summer
http get :3000/art/cieran/1217/Summer
http get :3000/characteristic/cieran/1217/Summer
```

# GET graph calls

To view the graphs as stored in STM 

```sh
http get :3000/graph
http get :3000/schema
http get :3000/res
```

To get an individual character before in-game advancement.

```sh
http get :3000/graph/gamestart/cieran
http get :3000/graph/initial/cieran
```
