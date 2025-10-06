# DnD Critical Event Calculator

The **Dungeons & Dragons Critical Event Calculator** is a terminal-based Erlang program that simulates dice rolls (d20), detects critical successes or failures, and generates randomized narrative outcomes depending on the weapon or tool used (axe, sword, mace, spear, or magic).

It demonstrates core Erlang concepts including pattern matching, recursion, guards, lists, lambdas, and `case of` blocks. It also includes an interactive REPL (`play/0`) where users can select tools and simulate rolls in real time.

---

## Instructions for Build and Use

### Steps to build and/or run the software:

1. Install [Erlang/OTP](https://www.erlang.org/downloads) and [rebar3](https://rebar3.org/).
2. Clone this repository: [Link](https://github.com/BrendanWillis/Crit-Calculator)
3. In the terminal, navigate to the project directory.
4. Compile the project:
   ```bash
   rebar3 compile
