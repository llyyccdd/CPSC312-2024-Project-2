% alphabet only lower case.
alphabet([a,b,c,d,e,f,g,h,i,j,k,l,m,n,o,p,q,r,s,t,u,v,w,x,y,z]).


% generates a decipher for decryption based on the keyword
generate_mnemonic_phrase_decipher(Keyword, Decipher) :-
    generate_mnemonic_phrase_cipher(Keyword, Cipher),
    alphabet(Alphabet),
    findall(DecipherChar, 
        (
            member(AlphabetChar, Alphabet), 
            nth0(Index, Cipher, AlphabetChar), 
            nth0(Index, Alphabet, DecipherChar)
        ), 
        Decipher).


generate_caesar_decipher(Keyword, Decipher) :-
    generate_mnemonic_phrase_cipher(Keyword, Cipher),
    alphabet(Alphabet),
    findall(DecipherChar, 
        (
            member(AlphabetChar, Alphabet), 
            nth0(Index, Cipher, AlphabetChar), 
            nth0(Index, Alphabet, DecipherChar)
        ), 
        Decipher).

% decrypt_by_mnemonic_phrase(Encrypted_message, Keyword, message) takes in a encrypted message and a keyword and decrypt the message message.
decrypt_by_mnemonic_phrase(Encrypted_message, Keyword, Decrypted_message) :-
    string_chars(Keyword, List_key),
    remove_duplicate(List_key, Key_nodup1),
    remove_chars(Key_nodup1, [' ', ',', '.', ';', ':', '?', '!'], Key_nodup),
    generate_mnemonic_phrase_decipher(Key_nodup, Decipher),
    string_chars(Encrypted_message, List_Encrypted),
    replace_chars(List_Encrypted, Decipher, Decrypted),
    string_chars(Decrypted_message, Decrypted), !.

decrypt_by_caesar(Encrypted_message, Int, Decrypted_message) :-
    generate_caesar_decipher(Int, Decipher),
    string_chars(Encrypted_message, List_Encrypted),
    replace_chars(List_Encrypted, Decipher, Decrypted),
    string_chars(Decrypted_message, Decrypted), !.


% tests:
% decrypt_by_mnemonic_phrase("tibbj vjpbw", "meowing in the wind", X). -> X = "tibbj vjpbw".
% decrypt_by_mnemonic_phrase("raddk wkods", "this is a seceret message", X). -> X = "raddk wkods".
% decrypt_by_caesar("mjqqt btwqi", 5, X). -> X = "nkrru cuxrj".

% generate_mnemonic_phrase_cipher(Keyword, Cipher) generates a monoalphabetic cipher based on a keyword
generate_mnemonic_phrase_cipher(Keyword, Cipher) :-
    string_chars(Keyword, List),       
    remove_duplicate(List, No_dup), 
    alphabet(Alphabet),
    sort_list(No_dup, Slist),
    remove_chars(Alphabet, Slist, Remaining),
    append(No_dup, Remaining, Cipher), !.


% generate_shifted_cipher(Int, Cipher) generates a shifted cipher from an integer
generate_caesar_cipher(Int, Cipher) :-
    alphabet(Alphabet),
    length(Alphabet, Len),
    findall(ShiftedChar,
        (
            nth0(Index, Alphabet, _),  % Replace Char with _ to indicate it's unused
            ShiftedIndex is (Index + Int) mod Len,
            nth0(ShiftedIndex, Alphabet, ShiftedChar)
        ),
        Cipher).


% encrypt_by_mnemonic_phrase(Message, Keyword, Encrypted_message) takes in a message and a keyword and generates the encrypted message.
encrypt_by_mnemonic_phrase(Message, Keyword, Encrypted_message) :-
    string_chars(Keyword, List_key),
    remove_duplicate(List_key, Key_nodup1),
    remove_chars(Key_nodup1, [' ', ',', '.', ';', ':', '?', '!'], Key_nodup),
    generate_mnemonic_phrase_cipher(Key_nodup, Cipher),
    string_chars(Message, List_Message),
    replace_chars(List_Message, Cipher, Encrypted),
    string_chars(Encrypted_message ,Encrypted), !.


encrypt_by_caesar(Message, Int, Encrypted_message) :-
    generate_caesar_cipher(Int, Cipher),
    string_chars(Message, List_Message),
    replace_chars(List_Message, Cipher, Encrypted),
    string_chars(Encrypted_message ,Encrypted), !.


% tests:
% encrypt_by_mnemonic_phrase("hello world", "meowing in the wind", X). -> X = "tibbj vjpbw".
% encrypt_by_mnemonic_phrase("hello world", "this is a seceret message", X). -> X = "raddk wkods".
% encrypt_by_caesar("hello world", 5, X). -> X = "mjqqt btwqi".


% remove_duplicate(L, UniqueL). UniqueL is L without the duplicated letters
remove_duplicate(L, UniqueL) :-
    remdup_acc(L, [], UniqueL).

remdup_acc([], Acc, Acc).
remdup_acc([H|T], Acc, UniqueL) :-
    (   member(H, Acc)
    ->  remdup_acc(T, Acc, UniqueL)
    ;   append(Acc, [H], NewAcc),
        remdup_acc(T, NewAcc, UniqueL)
    ).

% tests
% remove_duplicate([a,b,a,c,b,b,e],X).
% remove_duplicate([a],X).
% remove_duplicate([a,a],X).


% tests:
% generate_cipher([m, e, o, w, i, n, g, t, h, d], X). -> X = [m, e, o, w, i, n, g, t, h, d, a, b, c, f, j, k, l, p, q, r, s, u, v, x, y, z] .
% generate_cipher([t, h, i, s, a, e, c, r, m, g],X). -> X = [t, h, i, s, a, e, c, r, m, g, b, d, f, j, k, l, n, o, p, q, u, v, w, x, y, z].


% remove_chars(lst1, lst2, R) -> removes characters of lst2 from lst1
remove_chars([], _, []).
remove_chars([H|T], [], [H|T]) :- !.
remove_chars([H|T], [H|Rest], Result) :-
    remove_chars(T, Rest, Result), !.
remove_chars([H|T1], [X|T2], [H|Result]) :-
    dif(H, X),
    remove_chars(T1, [X|T2], Result).

% tests:
% remove_chars( [t, h, i, s, ' ', a, e, c, r, m, g], [' ', ',', '.', ';', ':', '?', '!'], Key_nodup). -> Key_nodup = [t, h, i, s, a, e, c, r, m, g].
% remove_chars([m, e, o, w, i, n, g, ' ', t, h, d], [' ', ',', '.', ';', ':', '?', '!'], Key_nodup). -> Key_nodup = [m, e, o, w, i, n, g, t, h, d].

% sort_list(List, Sorted) -> generates a sorted list
sort_list([], []).
sort_list([X], [X]).
sort_list(List, Sorted) :-
    List = [_|_], % Ensure List is non-empty
    split(List, Left, Right),
    sort_list(Left, SortedLeft),
    sort_list(Right, SortedRight),
    merge(SortedLeft, SortedRight, Sorted), !.
split(List, Left, Right) :-
    length(List, Len),
    HalfLen is Len // 2, % Integer division
    length(Left, HalfLen),
    append(Left, Right, List).
merge([], Right, Right).
merge(Left, [], Left) :- Left \= [].
merge([L|Ls], [R|Rs], [L|Sorted]) :- L @=< R, merge(Ls, [R|Rs], Sorted).
merge([L|Ls], [R|Rs], [R|Sorted]) :- L @> R, merge([L|Ls], Rs, Sorted).

% tests:
% sort_list([e,h,a,x,u,c],X). -> X = [a, c, e, h, u, x].
% sort_list([1,2,5,9,4,5],X). -> X = [1, 2, 4, 5, 5, 9].

% replace_chars(L1, L2, R) replaces characters of L1 with the characters of L2(the alphabet in lower case) so that each nth character of alphabet
replace_chars([], _, []).
replace_chars([Char|Chars], Replacements, [Replacement|Result]) :-
    (   char_in_alphabet(Char) ->
        % Convert the character to its corresponding index in the alphabet
        % Find the index of Char in the alphabet list
        alphabet(Alphabet),
        nth0(Index, Alphabet, Char),
        % Retrieve the replacement character from the list
        nth0(Index, Replacements, Replacement)
    ;   % If the character is not in the alphabet, replace it with itself
        Replacement = Char
    ),
    replace_chars(Chars, Replacements, Result).

% Check if a character is in the alphabet
char_in_alphabet(Char) :-
    alphabet(Alphabet),
    member(Char, Alphabet).


% nth0(Index, List, Element) fsinds the zero-based Index of Element in List
nth0(0, [Element|_], Element).
% Recursive case: Element is not the head, so we continue searching
nth0(Index, [_|Rest], Element) :-
    % Decrease Index and continue searching in the rest of the list
    nth0(IndexRest, Rest, Element),
    % Increment IndexRest to match the actual index in the original list
    Index is IndexRest + 1.


% tests:
% replace_chars([h, e, l, l, o, ' ', w, o, r, l, d],  [m, e, o, w, i, n, g, t, h, d, a, b, c, f, j, k, l, p, q, r, s, u, v, x, y, z], X). -> X = [t, i, b, b, j, ' ', v, j, p, b, w].
% replace_chars([h, e, l, l, o, ' ', w, o, r, l, d],[t, h, i, s, a, e, c, r, m, g, b, d, f, j, k, l, n, o, p, q, u, v, w, x, y, z], X). -> X = [r, a, d, d, k, ' ', w, k, o, d, s].
