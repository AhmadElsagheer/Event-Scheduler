% =================================
% A. The Structure of the Schedule
% =================================
% Schedule = a list of event timings
%          = [ event(Group, Course_Code, Event_Name, Event_Type,
%                   timing(Week_Number, Week_day, Day_slot)) ]
% Event timing includes:
%      1. Event defined by group, name, type and course code.
%      2. Timing defined by week number, day and slot.

% ==================
% B. Schedule Facts
% ==================
% Define course events, groups, quizslots, holidays and precedence constraints.

event_in_course(csen403, labquiz1, assignment).
event_in_course(csen403, labquiz2, assignment).
event_in_course(csen403, project1, evaluation).
event_in_course(csen403, project2, evaluation).
event_in_course(csen403, quiz1, quiz).
event_in_course(csen403, quiz2, quiz).
event_in_course(csen403, quiz3, quiz).

event_in_course(csen401, quiz1, quiz).
event_in_course(csen401, quiz2, quiz).
event_in_course(csen401, quiz3, quiz).
event_in_course(csen401, milestone1, evaluation).
event_in_course(csen401, milestone2, evaluation).
event_in_course(csen401, milestone3, evaluation).

event_in_course(csen402, quiz1, quiz).
event_in_course(csen402, quiz2, quiz).
event_in_course(csen402, quiz3, quiz).

event_in_course(math401, quiz1, quiz).
event_in_course(math401, quiz2, quiz).
event_in_course(math401, quiz3, quiz).

event_in_course(elct401, quiz1, quiz).
event_in_course(elct401, quiz2, quiz).
event_in_course(elct401, quiz3, quiz).
event_in_course(elct401, assignment1, assignment).
event_in_course(elct401, assignment2, assignment).

event_in_course(csen601, quiz1, quiz).
event_in_course(csen601, quiz2, quiz).
event_in_course(csen601, quiz3, quiz).
event_in_course(csen601, project, evaluation).

event_in_course(csen603, quiz1, quiz).
event_in_course(csen603, quiz2, quiz).
event_in_course(csen603, quiz3, quiz).

event_in_course(csen602, quiz1, quiz).
event_in_course(csen602, quiz2, quiz).
event_in_course(csen602, quiz3, quiz).

event_in_course(csen604, quiz1, quiz).
event_in_course(csen604, quiz2, quiz).
event_in_course(csen604, quiz3, quiz).
event_in_course(csen604, project1, evaluation).
event_in_course(csen604, project2, evaluation).

holiday(3, monday).
holiday(5, tuesday).
holiday(10, sunday).

studying(csen403, group4MET).
studying(csen401, group4MET).
studying(csen402, group4MET).
studying(math401, group4MET).
studying(elct401, group4MET).

studying(csen601, group6MET).
studying(csen602, group6MET).
studying(csen603, group6MET).
studying(csen604, group6MET).

should_precede(csen403,project1,project2).
should_precede(csen403,quiz1,quiz2).
should_precede(csen403,quiz2,quiz3).

quizslot(group4MET, tuesday, 1).
quizslot(group4MET, tuesday, 5).
quizslot(group4MET, thursday, 3).
quizslot(group6MET, saturday, 5).
quizslot(group6MET, wednesday, 4).

day(saturday, 1).
day(sunday, 2).
day(monday, 3).
day(tuesday, 4).
day(wednesday, 5).
day(thursday, 6).

% =====================
% C. Schedule Checkers
% =====================
% Check all constraints related to event timing assignment.

% succeeds only if all the should_precede facts of the courses
% studied by the group G are satisfied in Schedule.
precede(G, Schedule):-
                    event_timings(G, Schedule, ET),
                    event_precede(ET).

% succeeds only if timing T1 occurs before timing T2.
occur_before(T1, T2):-
                    T1 = timing(W1, Day1, S1), T2 = timing(W2, Day2, S2),
                    day(Day1, D1), day(Day2, D2),
                    (W1 < W2; (W1 = W2, D1 < D2); (W1 = W2, D1 = D2, S1 < S2)).

% event_precede(L) succeeds only if all events in L satisfy should_precede rules.
event_precede([]).
event_precede([ET_H|ET_T]):-
                    event_precede(ET_H, ET_T), event_precede(ET_T).

% event_precede(ET, L) succeeds only if event ET does not violate in of should_precede
% rules w.r.t the events in L.
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

% succeeds only if the group G does not have more than one event at the same
% timing in Schedule.
valid_slot_schedule(G, Schedule):-
                    event_timings(G, Schedule, ET),
                    no_concurrent_events(ET).

% no_concurrent_events(L) succeeds only if all events in L have distinct timings.
no_concurrent_events([]).
no_concurrent_events([ET_H|ET_T]):-
                    no_concurrent_events(ET_H, ET_T),
                    no_concurrent_events(ET_T).

% no_concurrent_events(ET, L) succeeds only if event ET occures in a timing
% different from all events in L.
no_concurrent_events(_, []).
no_concurrent_events(ET, [ET_H| ET_T]):-
                    ET = event(_, _, _, T1), ET_H = event(_, _, _, T2),
                    T1 \= T2, no_concurrent_events(ET, ET_T).


% succeeds only if group G does not have two quizzes for the
% same course in two consecutive weeks according to Schedule.
no_consec_quizzes(G, Schedule):-
                    event_timings(G, Schedule, ET),
                    no_consec_quizzes(ET).

% no_consec_quizzes(L) succeeds only if no two quizzes of the same
% course in L are in consecutive weeks.
no_consec_quizzes([]).

no_consec_quizzes([EH|ET]):-
                    EH = event(_, _, E_Type, _), E_Type \= quiz,
                    no_consec_quizzes(ET).

no_consec_quizzes([EH|ET]):-
                    EH = event(_, _, quiz, _),
                    quiz_not_consec_quizzes(EH, ET),
                    no_consec_quizzes(ET).

% no_consec_quizzes(E, L) succeeds only if E a quiz that
% is not consecutive to any quiz of the same course in L.
quiz_not_consec_quizzes(_, []).
quiz_not_consec_quizzes(E, [EH|ET]):-
                    EH = event(_, _, E_Type, _), E_Type \= quiz,
                    quiz_not_consec_quizzes(E, ET).

quiz_not_consec_quizzes(E, [EH|ET]):-
                    E = event(C1, _, _, _),
                    EH = event(C2, _, quiz, _), C1 \= C2,
                    quiz_not_consec_quizzes(E, ET).

quiz_not_consec_quizzes(E, [EH|ET]):-
                    E = event(C, _, _, timing(W1, _, _)),
                    EH = event(C, _, quiz, timing(W2, _, _)),
                    X is W1 - W2, abs(X, Y), Y > 1,
                    quiz_not_consec_quizzes(E, ET).

% succeeds only if group G does not have two quizzes scheduled on the same
% day according to Schedule.
no_same_day_quiz(G, Schedule):-
                    event_timings(G, Schedule, ET),
                    no_same_day_quiz(ET).

% no_same_day_quiz(L) succeeds only if no two quizzes in L occurs on the same day.
no_same_day_quiz([]).

no_same_day_quiz([ET_H|ET_T]):-
                    ET_H = event(_, _, E_Type, _), E_Type \= quiz,
                    no_same_day_quiz(ET_T).

no_same_day_quiz([ET_H|ET_T]):-
                    ET_H = event(_, _, quiz, _),
                    quiz_not_same_day_quizzes(ET_H, ET_T),
                    no_same_day_quiz(ET_T).

% quiz_not_same_day_quizzes(E, L) succeeds only if E is a quiz that
% does not occur on the same day with any other quiz in L.
quiz_not_same_day_quizzes(_, []).

quiz_not_same_day_quizzes(ET, [ET_H|ET_T]):-
                    ET_H = event(_, _, E_Type, _), E_Type \= quiz,
                    quiz_not_same_day_quizzes(ET, ET_T).

quiz_not_same_day_quizzes(ET, [ET_H|ET_T]):-
                    ET = event(_, _, _, timing(W1, D1, _)),
                    ET_H = event(_, _, quiz, timing(W2, D2, _)),
                    (W1 \= W2 ; (W1 = W2, D1 \= D2)),
                    quiz_not_same_day_quizzes(ET, ET_T).

% succeeds only if group G does not have two assignments scheduled on the same
% day according to Schedule.
no_same_day_assignment(G, Schedule):-
                    event_timings(G, Schedule, ET),
                    no_same_day_assignment(ET).


% no_same_day_assignment(L) succeeds only if no two assignments in L occurs on the same day.
no_same_day_assignment([]).

no_same_day_assignment([ET_H|ET_T]):-
                    ET_H = event(_, _, E_Type, _), E_Type \= assignment,
                    no_same_day_assignment(ET_T).

no_same_day_assignment([ET_H|ET_T]):-
                    ET_H = event(_, _, assignment, _),
                    ass_not_same_day_assignments(ET_H, ET_T),
                    no_same_day_assignment(ET_T).

% ass_not_same_day_assignments(E, L) succeeds only if E is an assignment
% that does not occur on the same day with any other assignment in L.
ass_not_same_day_assignments(_, []).

ass_not_same_day_assignments(ET, [ET_H|ET_T]):-
                    ET_H = event(_, _, E_Type, _), E_Type \= assignment,
                    ass_not_same_day_assignments(ET, ET_T).

ass_not_same_day_assignments(ET, [ET_H|ET_T]):-
                    ET = event(_, _, _, timing(W1, D1, _)),
                    ET_H = event(_, _, assignment, timing(W2, D2, _)),
                    (W1 \= W2 ; (W1 = W2, D1 \= D2)),
                    ass_not_same_day_assignments(ET, ET_T).

% succeeds only if Schedule has no events scheduled in any of
% the specified holidays.
no_holidays(G, Schedule):-
                    event_timings(G, Schedule, ET),
                    no_holidays(ET).

% no_holidays(L) succeeds only if no event in L is held on a holiday.
no_holidays([]).

no_holidays([ET_H|ET_T]):-
                    ET_H = event(_, _, _, timing(W, D, _)),
                    \+holiday(W, D), no_holidays(ET_T).

% =======================
% D. Schedule Generators
% =======================
% Generate or filter some lists.

% event_timings(G, S, ET) succeeds only if ET contains all event timings in
% schedule S that are related to group G.
event_timings(_, [], []).
event_timings(G, Schedule, Event_Timings):-
                    Schedule = [ET_H | ET_T],
                    ET_H = event(G, C, E_Name, E_Type, Timing),
                    ET = event(C, E_Name, E_Type, Timing),
                    event_timings(G, ET_T, R),
                    Event_Timings = [ET|R].

event_timings(G, Schedule, Event_Timings):-
                    Schedule = [ET_H | ET_T],
                    ET_H = event(G1, _, _, _, _),
                    G1 \= G,
                    event_timings(G, ET_T, Event_Timings).

% succeeds only if Timings contains all timings (pairs of day + slot)
% at which group G can have events.
available_timings(G, Timings):-
                    setof(timing(D, S), quizslot(G, D, S), Timings).

% succeeds only if Events contains all events that group G has.
group_events(G, []):-
                    \+group_events(G, _, _, _).
group_events(G, Events):-
                    setof(event(C, E_Name, E_Type), group_events(G, C, E_Name, E_Type), Events).

group_events(G, Course_Code, Event_Name, Event_Type):-
                    studying(Course_Code, G),
                    event_in_course(Course_Code, Event_Name, Event_Type).

% generates a timing (week + day + slot) where the week of
% the timing is between Week_Number and Weeks inclusive.
% This predicate is used to generate all possible timings
% to be used later for event timing assignment excluding Fridays.
find_timing(Week_Number, Weeks, W, D, S):-
                    Week_Number =< Weeks, W = Week_Number, day(D, _), member(S, [1, 2, 3, 4, 5]).

find_timing(Week_Number, Weeks, W, D, S):-
                    Week_Number < Weeks, Next_Week is Week_Number + 1,
                    find_timing(Next_Week, Weeks, W, D, S).

% ============================
% E. Schedule Main Predicates
% ============================
% Generate a schedule for all groups according to the specified
% number of weeks and satisfying all constraints.

% succeeds only if Schedule contains all events associated with their
% assigned timing where the number of weeks is at most Weeks.
schedule(Weeks,Schedule):-
                    setof(event(G, C, E_Name, E_Type),
                                        group_events(G, C, E_Name, E_Type), All_Events),
                    bagof(timing(W, D, S), find_timing(1, Weeks, W, D, S), All_Timings),
                    schedule(All_Events,All_Timings,[],Schedule).

% succeeds only if F_Schedule contains event timings for all events in All_Events
% such that every event will be assigned a timing from All_Timings and all
% constraints will be satisfied.
schedule([],_, S, S).

schedule(All_Events, All_Timings, C_Schedule, F_Schedule):-
                    All_Events = [E_H|E_T],
                    member(T,All_Timings),
                    E_H = event(G,C,E_Name,E_Type),
                    ET = event(G,C,E_Name,E_Type,T),
                    T = timing(_, D, S),
                    available_timings(G, Quizslots),
                    member(timing(D, S), Quizslots),
                    append(C_Schedule, [ET], N_Schedule),
                    precede(G,N_Schedule),
                    valid_slot_schedule(G,N_Schedule),
                    no_consec_quizzes(G,N_Schedule),
                    no_same_day_quiz(G,N_Schedule),
                    no_same_day_assignment(G,N_Schedule),
                    no_holidays(G,N_Schedule),
                    schedule(E_T,All_Timings,N_Schedule,F_Schedule).
