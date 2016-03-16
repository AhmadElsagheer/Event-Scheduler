% Schedule = [ group_schedule(Group, [event(Course_Code, Event_Name, Event_Type,
%               timing(Week_Number, Week_day, Day_slot))]) ]

% Returns event timings of G according to Schedule
event_timings(G, Schedule, Event_Timings):-
                    Schedule = [GS_H | _],
                    GS_H = group_schedule(G, Event_Timings).

event_timings(G, Schedule, Event_Timings):-
                    Schedule = [GS_H|GS_T],
                    GS_H \= group_schedule(G, _),
                    event_timings(G, GS_T, Event_Timings).


precede(G, Schedule):-
                    event_timings(G, Schedule, ET),
                    event_precede(ET).

occur_before(T1, T2):-
                    T1 = timing(W1, Day1, S1), T2 = timing(W2, Day2, S2),
                    day(Day1, D1), day(Day2, D2),
                    (W1 < W2; (W1 = W2, D1 < D2); (W1 = W2, D1 = D2, S1 < S2)).


% Check if the list of Event timings satisfy should_precede facts
event_precede([]).
event_precede([ET_H|ET_T]):-
                    event_precede(ET_H, ET_T), event_precede(ET_T).


% Check if event ET satisfies should_precede w.r.t the rest of the Event timing list
event_precede(_, []).
event_precede(ET, [ET_H|ET_T]):-
                    ET   = event(C, E1, _, T1),
                    ET_H = event(C, E2, _, T2),
                    (\+should_precede(C, E1, E2);
                    (should_precede(C, E1, E2), occur_before(T1, T2))),
                    (\+should_precede(C, E2, E1);
                    (should_precede(C, E2, E1), occur_before(T2, T1))),
                    event_precede(ET, ET_T).

event_precede(ET, [ET_H|ET_T]):-
                    ET   = event(C1, _, _, _),
                    ET_H = event(C2, _, _, _),
                    C1 \= C2, event_precede(ET, ET_T).


valid_slot_schedule(G, Schedule):-
                    event_timings(G, Schedule, ET),
                    no_concurrent_events(ET).

no_concurrent_events([]).
no_concurrent_events([ET_H|ET_T]):-
                    no_concurrent_events(ET_H, ET_T),
                    no_concurrent_events(ET_T).

no_concurrent_events(_, []).
no_concurrent_events(ET, [ET_H| ET_T]):-
                    ET = event(_, _, _, T1), ET_H = event(_, _, _, T2),
                    T1 \= T2, no_concurrent_events(ET, ET_T).

available_timings(G, Timings):-
                    setof(timing(D, S), quizslot(G, D, S), Timings).


group_events(G, Events):-
                    setof(event(C, E_Name, E_Type), group_events(G, C, E_Name, E_Type), Events).

group_events(G, Course_Code, Event_Name, Event_Type):-
                    studying(Course_Code, G),
                    event_in_course(Course_Code, Event_Name, Event_Type).

no_consec_quizzes(G, Schedule):-
                    event_timings(G, Schedule, ET),
                    no_consec_quizzes(ET).

no_consec_quizzes([]).

no_consec_quizzes([EH|ET]):-
                    EH = event(_, _, E_Type, _), E_Type \= quiz,
                    no_consec_quizzes(ET).

no_consec_quizzes([EH|ET]):-
                    EH = event(_, _, quiz, _),
                    quiz_not_consec_quizzes(EH, ET),
                    no_consec_quizzes(ET).

quiz_not_consec_quizzes(_, []).
quiz_not_consec_quizzes(E, [EH|ET]):-
                    EH = event(_, _, E_Type, _), E_Type \= quiz,
                    quiz_not_consec_quizzes(E, ET).

quiz_not_consec_quizzes(E, [EH|ET]):-
                    E = event(_, _, _, timing(W1, _, _)),
                    EH = event(_, _, quiz, timing(W2, _, _)),
                    X is W1 - W2, abs(X, Y), Y > 1,
                    quiz_not_consec_quizzes(E, ET).

no_same_day_quiz(G, Schedule):-
                    event_timings(G, Schedule, ET),
                    no_same_day_quiz(ET).

no_same_day_quiz([]).

no_same_day_quiz([ET_H|ET_T]):-
                    ET_H = event(_, _, E_Type, _), E_Type \= quiz,
                    no_same_day_quiz(ET_T).

no_same_day_quiz([ET_H|ET_T]):-
                    ET_H = event(_, _, quiz, _),
                    no_same_day_quiz(ET_H, ET_T),
                    no_same_day_quiz(ET_T).

quiz_not_same_day_quizzes(_, []).

quiz_not_same_day_quizzes(ET, [ET_H|ET_T]):-
                    ET_H = event(_, _, E_Type, _), E_Type \= quiz,
                    quiz_not_same_day_quizzes(ET, ET_T).

quiz_not_same_day_quizzes(ET, [ET_H|ET_T]):-
                    ET = event(_, _, _, timing(W1, D1, _)),
                    ET_H = event(_, _, quiz, timing(W2, D2, _)),
                    (W1 \= W2 ; (W1 = W2, D1 \= D2)),
                    quiz_not_same_day_quizzes(ET, ET_T).


no_same_day_assignment(G, Schedule):-
                    event_timings(G, Schedule, ET),
                    no_same_day_assignment(ET).

no_same_day_assignment([]).

no_same_day_assignment([ET_H|ET_T]):-
                    ET_H = event(_, _, E_Type, _), E_Type \= assignment,
                    no_same_day_assignment(ET_T).

no_same_day_assignment([ET_H|ET_T]):-
                    ET_H = event(_, _, assignment, _),
                    no_same_day_assignment(ET_H, ET_T),
                    no_same_day_assignment(ET_T).

ass_not_same_day_assignments(_, []).

ass_not_same_day_assignments(ET, [ET_H|ET_T]):-
                    ET_H = event(_, _, E_Type, _), E_Type \= assignment,
                    ass_not_same_day_assignments(ET, ET_T).

ass_not_same_day_assignments(ET, [ET_H|ET_T]):-
                    ET = event(_, _, _, timing(W1, D1, _)),
                    ET_H = event(_, _, assignment, timing(W2, D2, _)),
                    (W1 \= W2 ; (W1 = W2, D1 \= D2)),
                    ass_not_same_day_assignments(ET, ET_T).

no_holidays(G, Schedule):-
                    event_timings(G, Schedule, ET),
                    no_holiday(ET).

no_holidays([]).

no_holidays([ET_H|ET_T]):-
                    ET_H = event(_, _, _, timing(W, D, _)),
                    \+holiday(W, D), no_holiday(ET_T).

schedule(Week_Number, Schedule):-
                    event_timings(G, Schedule, ET),
                    group_events(G, E), precede(G, Schedule),
                    valid_slot_schedule(G, Schedule),
                    available_timings(G, L),
                    no_consec_quizzes(G, Schedule),
                    no_same_day_quiz(G, Schedule),
                    no_same_day_assignment(G, Schedule),
                    no_holidays(G, Schedule).
