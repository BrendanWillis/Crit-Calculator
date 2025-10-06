%% ------------------------------------------------------------
%% Critical Event Calculator – Phase 2: outcomes + case-of
%% ------------------------------------------------------------
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
roll_d20() ->
    rand:uniform(20).

outcome_type(Roll) when is_integer(Roll), Roll >= 1, Roll =< 20 ->
    case Roll of
        1  -> failure;
        20 -> success;
        _  -> normal
    end.

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

%% ============================================================
%% Outcomes 
%% ============================================================

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

%% ---------- Outcome selection ----------
crit_outcome(Tool, success) when is_atom(Tool) ->
    pick_random(outcome_list(Tool, success));
crit_outcome(Tool, failure) when is_atom(Tool) ->
    pick_random(outcome_list(Tool, failure));
crit_outcome(_BadTool, _BadType) ->
    "Invalid tool or crit type.".

outcome_list(axe, success) ->
    [
      "You cleave through defenses; target is sundered and staggered.",
      "Your axe bites deep—extra momentum frightens nearby foes."
    ];
outcome_list(axe, failure) ->
    [
      "You misjudge the range and strike with the haft; the handle cracks—Dex save to avoid dropping the head.",
      "Your swing overextends; you are off-balance until your next turn."
    ];
outcome_list(sword, success) ->
    [
      "Perfect thrust—double damage and disarm attempt.",
      "Flawless riposte—gain a free 5-ft reposition."
    ];
outcome_list(sword, failure) ->
    [
      "Your blade catches; you stumble—attack at disadvantage next turn.",
      "Pommel strike whiffs; you nick yourself for minor damage."
    ];
outcome_list(mace, success) ->
    [
      "Crushing blow—target’s armor dents; their movement is reduced.",
      "Skull-rattler—target must save or be stunned briefly."
    ];
outcome_list(mace, failure) ->
    [
      "You clip your own shin—ow. Take minor damage.",
      "Swing goes wide; mace slips—use an action to recover."
    ];
outcome_list(spear, success) ->
    [
      "Pinpoint lunge—target is pinned in place briefly.",
      "Whirl and strike—free shove 5 ft."
    ];
outcome_list(spear, failure) ->
    [
      "Spear lodges in terrain—lose your next attack to yank it free.",
      "Countered! You’re dragged a step off balance."
    ];
outcome_list(magic, success) ->
    [
      "Arc surges—spell echoes to a nearby foe for half effect.",
      "Perfect channel—spell DC increases for this cast."
    ];
outcome_list(magic, failure) ->
    [
      "Wild spark—roll on minor surge or take small feedback damage.",
      "Miscast—spell fizzles and you grant advantage to the target."
    ];
outcome_list(_Unknown, _Type) ->
    ["Unknown tool—no themed outcome available."].

pick_random([H]) -> H;
pick_random(List) when is_list(List), List =/= [] ->
    N = length(List),
    lists:nth(rand:uniform(N), List);
pick_random(_) ->
    "No outcomes defined.".

%% ------------------ play loop ------------------
play() ->
    seed(),
    io:format("Welcome to DnD Crit Simulator!~n"),
    loop().

loop() ->
    io:format("Choose: axe | sword | mace | spear | magic | force_succ | force_fail | q~n> "),
    case trim(io:get_line("")) of
        "q" -> io:format("Bye!~n"), ok;

        "force_succ" ->
            Tool = ask_tool(),
            io:format("~s~n", [lists:flatten(describe(force(Tool, success)))]),
            loop();

        "force_fail" ->
            Tool = ask_tool(),
            io:format("~s~n", [lists:flatten(describe(force(Tool, failure)))]),
            loop();

        ToolStr ->
            case parse_tool(ToolStr) of
                {ok, Tool} ->
                    io:format("~s~n", [simulate_once_text(Tool)]),
                    loop();
                error ->
                    io:format("Unknown choice.~n"),
                    loop()
            end
    end.

ask_tool() ->
    io:format("Tool (axe|sword|mace|spear|magic): "),
    case parse_tool(trim(io:get_line(""))) of
        {ok, T} -> T;
        error   -> io:format("Unknown tool, defaulting to sword.~n"), sword
    end.

parse_tool(S) ->
    case string:lower(trim(S)) of
        "axe"   -> {ok, axe};
        "sword" -> {ok, sword};
        "mace"  -> {ok, mace};
        "spear" -> {ok, spear};
        "magic" -> {ok, magic};
        _       -> error
    end.

trim(Str) when is_list(Str) ->
    string:trim(Str).
