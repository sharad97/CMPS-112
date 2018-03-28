
% Sharad Shrestha
% 1439935 (sshrest3)
% CMPS 112
% Asg 4

not( X ) :- X, !, fail.
not( _ ).

% Modified haversine.perl provided by professor.
dm_rads( degmin( Degs, Mins ), Rads ) :-
    Min1 is Mins / 60,
    Degs1 is Degs + Min1,
    Degs2 is pi / 180,
    Rads is Degs1 * Degs2.

% Modified funtions.pl provided by professor for haversine_radians.
haversine_radians( Lat1, Lon1, Lat2, Lon2, Travel_Dist ) :-
    dm_rads( Lat1, LatO ),
    dm_rads( Lat2, LatT ),
    dm_rads( Lon1, LonO ),
    dm_rads( Lon2, LonT ), 
    Dlon is LonT - LonO,
    Dlat is LatT - LatO,
    A1 is cos( LatO ),
    A2 is cos( LatT ),
    A3 is sin( Dlon / 2 ),
    A4 is sin( Dlat / 2 ),
    A is A4 ** 2
        + A1 * A2 * A3 ** 2,
    A5 is sqrt( A ),
    A6 is sqrt( 1 - A ), 
    A7 is atan2( A5, A6),   
    Travel_Dist is 2 * A7 * 3961. 

travel_dists( Airfield1, Airfield2, Travel_Dist ) :-
    airport( Airfield1, _, Lat1, Lon1 ),
    airport( Airfield2, _, Lat2, Lon2 ),
    haversine_radians( Lat1, Lon1, Lat2, Lon2, Travel_Dist).

travel_time(Travel_Dist, Time_travel) :-
    Time_travel is Travel_Dist / 500.
    
total_hrs( time( Hrs, Mins ), Total_Hrs ) :-
    Total_Hrs is Hrs + Mins / 60.

show_digits( Clocks ) :-
    Clocks < 10, 
    write( 0 ), 
    write( Clocks ).
    
show_digits( Clocks ) :-
    Clocks >= 10, 
    write( Clocks ).
    
hrs1((Hrs), Total_Hrs):-
    Clock_Num is floor( Total_Hrs * 60 ),
    Hrs is Clock_Num // 60.

mins1((Mins), Total_Hrs):-
    Clock_Num is floor( Total_Hrs * 60 ),
    Mins is Clock_Num mod 60.   
 
display_time( Total_Hrs ) :-
    hrs1((Hrs), Total_Hrs),
    mins1((Mins), Total_Hrs),
    show_digits( Hrs ), 
    print( ':' ), 
    show_digits( Mins ).

check1(Flight, Airfield, Depature, Land_Time, Depature_Time):-
    total_hrs( Depature_Time, Depature ),
    travel_dists( Flight, Airfield, Travel_Dist ),
    travel_time( Travel_Dist, FlightTime ),  
    Land_Time is Depature + FlightTime,
    Land_Time < (12.0 * 2).
    
check2(Flight, Connect, Depature, Land_Time, Depature_Time):-
    total_hrs( Depature_Time, Depature ),
    travel_dists( Flight, Connect, Travel_Dist ),
    travel_time( Travel_Dist, FlightTime ),  
    Land_Time is Depature + FlightTime,
    Land_Time < (12.0 * 2).

check3(ConnectDepature, Land_Time):-
    Nd is ConnectDepature,
    Lt is Land_Time,
    Half is 0.5,
    Period is Nd - Lt - Half,
    Period >= 0.

find_dest( Airfield, Airfield, _, [Airfield], _ ).  

% modified foxchicken.pl provided by professor.
find_dest( Flight, Airfield, Finished, 
    [[Flight, Depature, Land_Time] | List], 
    Depature_Time ) :-
    flight( Flight, Airfield, Depature_Time ),
    not( member( Airfield,Finished ) ),
    check1(Flight, Airfield, Depature, Land_Time, Depature_Time),
    find_dest( Airfield, Airfield, [Airfield |Finished], List, _).
    
find_dest( Flight, Airfield, Finished, 
    [[Flight, Depature, Land_Time] | List], 
    Depature_Time ) :-
    flight( Flight, Connect, Depature_Time ),
    not( member( Connect,Finished ) ),
    check2(Flight, Connect, Depature, Land_Time, Depature_Time),
    flight( Connect, _, ConnectDepature_Time ),
    total_hrs( ConnectDepature_Time, ConnectDepature ),
    check3(ConnectDepature, Land_Time),
    find_dest( Connect, Airfield, [Connect |Finished], 
               List, ConnectDepature_Time ).

writepath( [] ).

% modified graphpath.pl provided by professor.    
writepath( [[Take_Off, Depature_Time, Arrival_Time], 
    Landing | []] ) :-
    airport( Take_Off, From_Airfiled, _, _), 
    airport( Landing, To_Airfiled, _, _),
    nl,
    write( '     ' ), 
    write( 'Take_Off  ' ),
    write( 'Time: '),
    display_time( Depature_Time ),
    write( '  ' ),
    write( 'From: ' ),
    write('('),
    write( Take_Off ),
    write(')'),
    write( '  ' ),
    write( From_Airfiled ),
    nl,
    write( '     ' ), 
    write( 'Landing   ' ),
    write( 'Time: '),
    display_time( Arrival_Time ),
    write( '  ' ),    
    write( 'To:   ' ),
    write('('),
    write( Landing ),
    write(')'), 
    write( '  ' ),  
    write( To_Airfiled ),    
    nl,
    nl,
    !, 
    true.

writepath( [[Take_Off, Depature_Time, Arrival_Time], 
    [Landing, Depature_Time1, Arrival_Time1] | End] ) :-
    airport( Take_Off, From_Airfiled, _, _), 
    airport( Landing, To_Airfiled, _, _),
    nl,
    write( '     ' ), 
    write( 'Take_Off  ' ),
    write( 'Time: '),
    display_time( Depature_Time ),
    write( '  ' ),
    write( 'From: ' ),
    write('('),
    write( Take_Off ),
    write(')'),
    write( '  ' ),
    write( From_Airfiled ),
    nl,
    write( '     ' ), 
    write( 'Landing   ' ),
    write( 'Time: '),
    display_time( Arrival_Time ),
    write( '  ' ),    
    write( 'To:   ' ),
    write('('),
    write( Landing ),
    write(')'), 
    write( '  ' ),  
    write( To_Airfiled ),    
    nl,
    nl,
    !, 
    writepath( [[Landing, Depature_Time1, Arrival_Time1] | End] ).

fly( Take_Off, Take_Off ) :-
    nl,
    nl,
    write( '     ' ),
    write( 'Error!!!' ), 
    nl,
    write( '     ' ),
    write('You typed '),
    print(Take_Off),
    write( ' to '), 
    print(Take_Off), 
    write(' which is the same airport.'),
    nl,
    write( '     ' ),
    write('Depature and arrival airport must be different.'),
    nl,
    write( '     ' ),
    write( 'Please type differnt airports.' ),
    nl,
    nl,
    !, 
    fail.
    
fly( Take_Off, Landing ) :-
    airport( Take_Off, _, _, _ ),
    airport( Landing, _, _, _ ),

    find_dest( Take_Off, Landing, [Take_Off], List, _ ),
    !, 
    nl,
    writepath( List ),
    true.    

fly( Take_Off, Landing ) :-
    airport( Take_Off, _, _, _ ),
    airport( Landing, _, _, _ ),
    nl,
    nl,
    write( '     ' ),
    write( 'Error!!!' ), 
    nl,
    write( '     ' ),
    write( 'You typed ' ),
    print(Take_Off),
    write( ' to '), 
    print(Landing),
    write( ' which is currently not possible.' ), 
    nl,
    write( '     ' ),
    write( 'Please type differnt airports.' ),
    nl,
    nl,
    !, fail.

fly( _, _) :-
    nl,
    nl,
    write( '     ' ),
    write( 'Error!!!' ),
    nl,
    write( '     ' ),
    write('Unable to find the airport.'),
    nl,
    write( '     ' ),
    write('Please type different airports.'),
    nl,
    nl,
    !, fail.
