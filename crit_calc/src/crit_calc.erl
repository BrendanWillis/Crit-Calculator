%% Critical Event Calculator for Dungeons & Dragons
-module(crit_calc).
-export([
    seed/0,
    roll_d20/0,
    outcome_type/1,
    simulate_once/1,
    simulate_n/2,
    force/2,
    describe/1,
    simulate_once_text/1,
    simulate_n_text/2,
    play/0
]).

%% ---------- RNG ----------
seed() ->
    _ = rand:seed(exsplus,
                  {erlang:monotonic_time(),
                   erlang:unique_integer([monotonic]),
                   erlang:phash2(self())}),
    ok.

%% ---------- Dice ----------
roll_d20() -> rand:uniform(20).

outcome_type(Roll) when is_integer(Roll), Roll >= 1, Roll =< 20 ->
    case Roll of
        1  -> failure;
        20 -> success;
        _  -> normal
    end.

%% ---------- Simulation (data tuples) ----------
simulate_once(Tool) when is_atom(Tool) ->
    R = roll_d20(),
    {R, outcome_type(R), Tool}.

simulate_n(Tool, N) when is_atom(Tool), is_integer(N), N > 0 ->
    simulate_n_(Tool, N, []).

simulate_n_(_Tool, 0, Acc) ->
    lists:reverse(Acc);
simulate_n_(Tool, N, Acc) ->
    R = roll_d20(),
    simulate_n_(Tool, N - 1, [{R, outcome_type(R), Tool} | Acc]).

%% ---------- Manual forcing for tests ----------
force(Tool, success) when is_atom(Tool) -> {forced, success, Tool};
force(Tool, failure) when is_atom(Tool) -> {forced, failure, Tool}.

%% Outcomes & text formatting

describe({forced, Type, Tool}) when Type == success; Type == failure ->
    Msg = crit_outcome(Tool, Type),
    io_lib:format("[FORCED ~p] ~s", [Type, Msg]);
describe({Roll, normal, Tool}) ->
    io_lib:format("Roll ~p with ~p: no crit.", [Roll, Tool]);
describe({Roll, Type, Tool}) ->
    Msg = crit_outcome(Tool, Type),
    case Type of
        success ->
            io_lib:format("Roll ~p (CRIT SUCCESS) with ~p: ~s", [Roll, Tool, Msg]);
        failure ->
            io_lib:format("Roll ~p (CRIT FAILURE) with ~p: ~s", [Roll, Tool, Msg])
    end.

simulate_once_text(Tool) ->
    lists:flatten(describe(simulate_once(Tool))).

simulate_n_text(Tool, N) ->
    Tuples = simulate_n(Tool, N),
    LinesIolist = lists:map(fun(T) -> [describe(T), $\n] end, Tuples),
    lists:flatten(LinesIolist).

%% ---------- Flavor Text Outcomes (Humorous / Witty) ----------
crit_outcome(axe, success) ->
    pick_random([
        "Your axe finds its mark with stylish precision. You act like you meant to do that.",
        "A perfect strike! You pause, allowing witnesses a moment to appreciate your greatness."
    ]);
crit_outcome(axe, failure) ->
    pick_random([
        "You swing with enthusiasm and hit nothing. You pretend it was a ‘warning strike’.",
        "Your axe slips slightly. You recover quickly, hoping nobody noticed. They noticed."
    ]);

crit_outcome(sword, success) ->
    pick_random([
        "Your blade lands true, clean and efficient. You consider asking for a tip.",
        "A flawless strike! You nod humbly, as if this was standard procedure."
    ]);
crit_outcome(sword, failure) ->
    pick_random([
        "You lunge forward, immediately questioning that decision. It goes poorly.",
        "Your sword swing misses by a generous margin. Very generous."
    ]);

crit_outcome(mace, success) ->
    pick_random([
        "A satisfying crunch echoes. You try not to look too proud… you fail.",
        "Your mace thumps into its target with comedic finality. Nailed it."
    ]);
crit_outcome(mace, failure) ->
    pick_random([
        "You over-commit and nearly topple over. A solid ‘oops’ moment.",
        "Your mace attack drags on the ground. Not your finest swing."
    ]);

crit_outcome(spear, success) ->
    pick_random([
        "A clean pierce, direct and effective. You smugly pretend it was effortless.",
        "Your spear finds its target in one graceful motion. You hope someone was watching."
    ]);
crit_outcome(spear, failure) ->
    pick_random([
        "You poke at the air with great determination. The air remains uninjured.",
        "Your spear glances off harmlessly. You pretend you were just measuring distance."
    ]);

crit_outcome(magic, success) ->
    pick_random([
        "Your spell bursts forth impressively! You act like that was definitely the intended effect.",
        "Arcane energy lands perfectly. You suppress the urge to bow."
    ]);
crit_outcome(magic, failure) ->
    pick_random([
        "Your spell fizzles. You mutter something about ‘unstable ley lines’.",
        "Sparks fly… mostly in the wrong direction. You pretend it’s ‘experimental magic’."
    ]);

crit_outcome(_Tool, _Type) ->
    "Unknown tool — even the narrator is confused.".

%% Interactive menus 

play() ->
    seed(),
    io:format("Welcome to DnD Crit Simulator!~n"),
    loop_main().

%% ---- Main menu loop ----
loop_main() ->
    show_main_menu(),
    Choice = trim(io:get_line("> ")),
    case parse_choice_main(Choice) of
        {roll, Tool} ->
            io:format("~s~n", [simulate_once_text(Tool)]),
            loop_main();
        force_success ->
            choose_weapon_forced(success);
        force_failure ->
            choose_weapon_forced(failure);
        quit ->
            io:format("Goodbye!~n"), ok;
        error ->
            io:format("Invalid choice.~n"),
            loop_main()
    end.

show_main_menu() ->
    io:format("Choose your action:~n"),
    io:format("1) Roll with Axe~n"),
    io:format("2) Roll with Sword~n"),
    io:format("3) Roll with Mace~n"),
    io:format("4) Roll with Spear~n"),
    io:format("5) Roll with Magic~n"),
    io:format("6) Force a Critical Success~n"),
    io:format("7) Force a Critical Failure~n"),
    io:format("8) Quit~n").

parse_choice_main("1") -> {roll, axe};
parse_choice_main("2") -> {roll, sword};
parse_choice_main("3") -> {roll, mace};
parse_choice_main("4") -> {roll, spear};
parse_choice_main("5") -> {roll, magic};
parse_choice_main("6") -> force_success;
parse_choice_main("7") -> force_failure;
parse_choice_main("8") -> quit;
parse_choice_main(_)   -> error.

%% ---- Force flow: show weapon menu, apply forced result or quit ----
choose_weapon_forced(Type) when Type == success; Type == failure ->
    show_force_menu(),
    C = trim(io:get_line("> ")),
    case parse_choice_force(C) of
        {weapon, Tool} ->
            io:format("~s~n", [lists:flatten(describe(force(Tool, Type)))]),
            loop_main();
        quit ->
            io:format("Goodbye!~n"), ok;
        error ->
            io:format("Invalid choice.~n"),
            choose_weapon_forced(Type)
    end.

show_force_menu() ->
    io:format("Choose a weapon (or 6 to Quit):~n"),
    io:format("1) Axe~n"),
    io:format("2) Sword~n"),
    io:format("3) Mace~n"),
    io:format("4) Spear~n"),
    io:format("5) Magic~n"),
    io:format("6) Quit~n").

parse_choice_force("1") -> {weapon, axe};
parse_choice_force("2") -> {weapon, sword};
parse_choice_force("3") -> {weapon, mace};
parse_choice_force("4") -> {weapon, spear};
parse_choice_force("5") -> {weapon, magic};
parse_choice_force("6") -> quit;
parse_choice_force(_)   -> error.

%% ---------- small string helper ----------
trim(Str) when is_list(Str) ->
    string:trim(Str).
