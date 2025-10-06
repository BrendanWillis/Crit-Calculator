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
```
3. 
Choose one of the available tools:
axe, sword, mace, spear, or magic

Or use special commands:

force_succ → Force a critical success

force_fail → Force a critical failure

q → Quit the simulator
4. 
Each roll will display:

The result (1–20)

 The outcome type (normal / crit success / crit failure)

 A flavorful description depending on the tool & outcome

 ### Development enviroment
 ---

##  Development Environment

To recreate the development environment for this project, you will need the following:

###  Operating System
- macOS (tested on macOS 14+)
- Linux (Ubuntu / Debian-based recommended)
- Windows (via WSL 2 or native Erlang installation)

###  Required Software
| Tool        | Version   | Purpose                                     |
|------------|-----------|---------------------------------------------|
| Erlang/OTP | 28        | Core language and runtime                   |
| rebar3    | 3.25.1    | Build tool and dependency manager for Erlang |

###  Helpful Tools
- **Terminal / Shell** — for compiling and running the application.  
- **VS Code** with Erlang plugin *(optional)* — for easier code navigation and syntax highlighting.

### ⚡ Quick Setup Recap
```bash
# macOS setup example
brew install erlang rebar3

# Verify installation
erl -version
rebar3 --version

```


##  Useful Websites to Learn More

These resources were especially helpful while developing this project and learning Erlang:

- [ **Erlang Official Documentation**](https://www.erlang.org/docs)  
  The primary reference for Erlang syntax, standard libraries, and official guides.

- [ **Learn You Some Erlang for Great Good**](https://learnyousomeerlang.com/)  
  A beginner-friendly and humorous online book that explains Erlang concepts clearly and progressively.

- [ **Rebar3 User Guide**](https://rebar3.org/docs/)  
  Documentation for Erlang's build tool, used for compiling, running, and managing dependencies.

- [▶ **Erlang Programming Tutorials on YouTube**](https://www.youtube.com/results?search_query=erlang+programming+tutorial)  
  A collection of video tutorials that help visualize concepts and reinforce practical skills.

## ---

##  Future Work

I have not finished the entire program yet, but my immediate next steps are:

- [ ] Finish fully implementing the **interactive REPL** (`play/0`) so all options function smoothly.  
- [ ] Complete and refine the **critical outcome tables** for each weapon/tool.  
- [ ] Clean up and finalize the **code structure** to ensure everything runs without errors.
