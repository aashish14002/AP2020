-module(coordinator).
-behaviour(gen_statem).

-export([start/1]).
-export([terminate/3,init/1,callback_mode/0]).
-export([game/3, wait_for_player1_turn/3, wait_for_player2_turn/3, game_over/3 ]).

start(GameData) ->
    gen_statem:start_link(?MODULE, ?MODULE, GameData, []).


init(GameData) ->
    [Player1, Player2, Rounds, BrokerRef] = GameData,
    StateData = #{ 
                player1 => Player1,
                player2 => Player2,
                scores => {0, 0},
                roundNo => 0,
                gameLength => Rounds,
                lastMove => none,
                brokerRef => BrokerRef
        },
    {ok, game, StateData}.

callback_mode() ->
    state_functions.

game({call, {From, _}=F}, {move, Choice},
    #{player1 := {Player1, _R1} ,player2 := {Player2, _R2}} = StateData) ->
    case From of
        Player1 -> {next_state, wait_for_player2_turn, StateData#{player1 := F ,lastMove := Choice}};
        Player2 -> {next_state, wait_for_player1_turn, StateData#{player2 := F ,lastMove := Choice}};
        _ -> keep_state_and_data
    end.

wait_for_player1_turn({call, {From, _}=P1}, {move, Choice},
        #{player1 := {Player1, _R1} ,player2 := P2, scores := {Score1, Score2},
        roundNo := RoundNo, lastMove := LastMove} = StateData) ->
    case From of
        Player1 -> RoundNo = RoundNo + 1,
                case roundResult(Choice, LastMove) of
                    round_lost -> Score2 = Score2 + 1,
                                P1Res = round_lost,
                                P2Res = round_won,
                                reportResult(P1, P2, Score2, Score1, Score2, RoundNo, P1Res, P2Res, StateData);
                    round_won -> Score1 = Score1 + 1,
                                P2Res = round_lost,
                                P1Res = round_won,
                                reportResult(P1, P2, Score1, Score1, Score2, RoundNo, P1Res, P2Res, StateData);
                    _ -> keep_state_and_data
                end;
        _ -> keep_state_and_data
    end.

wait_for_player2_turn({call, {From, _}=P2}, {move, Choice},
        #{player2 := {Player2, _R2} ,player1 := P1, scores := {Score1, Score2},
        roundNo := RoundNo, lastMove := LastMove} = StateData) ->
case From of
    Player2 -> RoundNo = RoundNo + 1,
            case roundResult(Choice, LastMove) of
                round_lost -> Score1 = Score1 + 1,
                            P2Res = round_lost,
                            P1Res = round_won,
                            reportResult(P1, P2, Score1, Score1, Score2, RoundNo, P1Res, P2Res, StateData);
                round_won -> Score2 = Score2 + 1,
                            P1Res = round_lost,
                            P2Res = round_won,
                            reportResult(P1, P2, Score2, Score1, Score2, RoundNo, P1Res, P2Res, StateData);
                _ -> keep_state_and_data
            end;
    _ -> keep_state_and_data
end.

game_over(_, _, _) ->
    keep_state_and_data.

roundResult(P1choice, P2choice) ->
    case {P1choice, P2choice} of
        {P1choice, P1choice} -> round_tie;
        {rock, paper} -> round_lost;
        {rock, scissor} -> round_won;
        {paper, rock} -> round_won;
        {paper, scissor} -> round_lost;
        {scissor, rock} -> round_lost;
        {scissor, paper} -> round_won;
        _ -> R1 = lists:member(P1choice, [rock, paper, scissor]),
            R2 = lists:member(P1choice, [rock, paper, scissor]),
            if 
                R1 =:= true -> round_won;
                R2 =:= true -> round_lost;
                true -> round_tie
            end
    end.

reportResult(P1, P2, WinScore, Score1, Score2, RoundNo, P1Res, P2Res, StateData) ->
    #{gameLength := Rounds, brokerRef := BrokerRef} = StateData,
    case Rounds/2 < WinScore of
        true -> Result1 = {game_over, Score1, Score2},
                Result2 = {game_over, Score2, Score1},
                gen_server:cast(BrokerRef, {self(), game_over, RoundNo}),
                {next_state, game_over, StateData#{player1 := P1 ,lastMove := none,
                    roundNo := RoundNo, scores := {Score1, Score2}}, [{reply, P1, Result1}, {reply, P2, Result2}]};
        false -> {next_state, game, StateData#{player1 := P1 ,lastMove := none,
                    roundNo := RoundNo, scores := {Score1, Score2}}, [{reply, P1, P1Res}, {reply, P2, P2Res}]}
    end.


terminate(_Reason, _, _) ->
    ok.