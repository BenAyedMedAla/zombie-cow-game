# 🧟‍♂️ Zombie Cow Survival - Prolog Game

A simple terminal-based survival game written in **Prolog**, where the player (Dimitri) must survive on a grid filled with different types of cows, obstacles, and the threat of zombification. One of the cows is randomly turned into a zombie at the start, and the infection spreads. Your mission is to **stay alive as long as possible**.

---

## 📜 Game Description

You are **Dimitri**, placed randomly on a 10x10 grid along with:

- 🐮 **Vaches** of three breeds:
  - `brune`
  - `simmental`
  - `alpine_herens`
- 🪨 **Rochers** (rocks) — impassable obstacles
- 🌲 **Arbres** (trees) — also block movement
- 🧟 **Zombified cows** — spread infection to adjacent cows

### 🧠 Game Logic

- One cow is randomly zombified at the beginning.
- Each turn:
  1. You choose a direction to move (nord, sud, est, ouest, reste).
  2. All cows move randomly if the destination is not occupied.
  3. Zombified cows infect any adjacent (north, south, east, west) living cows.
  4. If you are next to a zombie cow, the game ends.

---

## ▶️ How to Run

Make sure you have **SWI-Prolog** installed.


1. Clone the repository:

```bash
git clone https://github.com/yourusername/zombie-cow-game.git
cd zombie-cow-game

2.Launch SWI-Prolog:
swipl

3.Load the game file:
?- [zombie_cow_game]

4.Start the game:
?- jouer.
You’ll be shown the game grid. Use direction inputs to move:
Direction (nord, sud, est, ouest, reste) ? nord.
