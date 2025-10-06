# DnD Critical Event Calculator (Update)

A terminal-based Erlang simulator that rolls a d20, detects critical successes and failures, and returns flavorful narrative outcomes based on the chosen tool (axe, sword, mace, spear, or magic).

## Instructions for Build and Use

Steps to build and/or run the software:

1. Install prerequisites: Erlang/OTP 28+ and rebar3.
2. Open a terminal in the project folder (the folder containing `rebar.config`):
   ```bash
   cd crit_calc
   ```
3. Build and launch an Erlang shell:
   ```bash
   rebar3 compile
   rebar3 shell
   ```

Instructions for using the software:

1. Seed the RNG:
   ```erlang
   crit_calc:seed().
   ```
2. Start the interactive simulator:
   ```erlang
   crit_calc:play().
   ```
3. At the prompt choose a tool (axe, sword, mace, spear, magic) or use special commands:
   - `force_succ` — force a critical success  
   - `force_fail` — force a critical failure  
   - `q` — quit the simulator

## Development Environment

To recreate the development environment, you need the following software and/or libraries with the specified versions:

* Erlang/OTP 28 (runtime and compiler)
* rebar3 (build tool) — e.g., 3.25.x
* Recommended: macOS 14+ or Linux (Ubuntu/Debian); Windows via WSL2

## Useful Websites to Learn More

I found these websites useful in developing this software:

* [Erlang Official Documentation](https://www.erlang.org/docs)
* [Learn You Some Erlang for Great Good](https://learnyousomeerlang.com/)
* [Rebar3 User Guide](https://rebar3.org/docs/)

## Future Work

The following items I plan to fix, improve, and/or add to this project in the future:

* [ ] Finish and polish the interactive REPL (`play/0`) so all options function smoothly
* [ ] Complete and refine critical outcome tables for each tool
* [ ] Add tests (EUnit/Common Test) and CI configuration

