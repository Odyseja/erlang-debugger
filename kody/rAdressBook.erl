
%%%-------------------------------------------------------------------
%%% @author Aleksandra Bańkowska
%%% @copyright (C) 2014, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 16. gru 2014 09:56
%%%-------------------------------------------------------------------
-module(rAdressBook).
-author("Aleksandra Bańkowska").

%% API
-export([start/0]).
-export([stop/0]).
-export([init/0]).
-export([crash/0]).

-export([addContact/2]).
-export([addEmail/3]).
-export([addPhone/3]).
-export([addCompany/3]).
-export([addPosition/3]).

-export([removeContact/2]).
-export([removeEmail/1]).
-export([removePhone/1]).

-export([getEmails/2]).
-export([getPhones/2]).

-export([findByEmail/1]).
-export([findByPhone/1]).
-export([findByCompany/1]).

start() -> register(server, spawn(?MODULE, init, [])).
stop() -> server ! stop.
crash() -> server ! crash.

init() ->
  AdressBook=adressBook:createAdressBook(),
  loop(AdressBook).

loop(AdressBook) ->
  receive
    {addContact, FirstName, LastName, Pid} -> NewAdressBook = adressBook:addContact(FirstName, LastName, AdressBook),
      checkNewAdressBook(NewAdressBook, AdressBook, Pid);
    {addEmail, FirstName, LastName, Email, Pid} -> NewAdressBook = adressBook:addEmail(FirstName, LastName, Email, AdressBook),
      checkNewAdressBook(NewAdressBook, AdressBook, Pid);
    {addPhone, FirstName, LastName, Phone, Pid} -> NewAdressBook = adressBook:addPhone(FirstName, LastName, Phone, AdressBook),
      checkNewAdressBook(NewAdressBook, AdressBook, Pid);
    {addCompany, FirstName, LastName, Company, Pid} -> NewAdressBook = adressBook:addCompany(FirstName, LastName, Company, AdressBook),
      checkNewAdressBook(NewAdressBook, AdressBook, Pid);
    {addPosition, FirstName, LastName, Position, Pid} -> NewAdressBook = adressBook:addPosition(FirstName, LastName, Position, AdressBook),
      checkNewAdressBook(NewAdressBook, AdressBook, Pid);
    {removeContact, FirstName, LastName, Pid} -> NewAdressBook = adressBook:removeContact(FirstName, LastName, AdressBook),
      checkNewAdressBook(NewAdressBook, AdressBook, Pid);
    {removeEmail, Email, Pid} -> NewAdressBook = adressBook:removeEmail(Email, AdressBook),
      checkNewAdressBook(NewAdressBook, AdressBook, Pid);
    {removePhone, Phone, Pid} -> NewAdressBook = adressBook:removePhone(Phone, AdressBook),
      checkNewAdressBook(NewAdressBook, AdressBook, Pid);

    {getEmails, FirstName, LastName, Pid} -> NewAdressBook = adressBook:getEmails(FirstName, LastName, AdressBook),
      finding(NewAdressBook, AdressBook, Pid);
    {getPhones, FirstName, LastName, Pid} -> NewAdressBook = adressBook:getPhones(FirstName, LastName, AdressBook),
      finding(NewAdressBook, AdressBook, Pid);

    {findByEmail, Email, Pid} -> NewAdressBook = adressBook:findByEmail(Email, AdressBook),
      finding(NewAdressBook, AdressBook, Pid);
    {findByPhone, Phone, Pid} -> NewAdressBook = adressBook:findByPhone(Phone, AdressBook),
      finding(NewAdressBook, AdressBook, Pid);
    {findByCompany, Company, Pid} -> NewAdressBook = adressBook:findByCompany(Company, AdressBook),
      finding(NewAdressBook, AdressBook, Pid);
    stop -> terminate();
    crash -> 1/0
  end.

terminate() -> ok.

checkNewAdressBook({error, Msg}, AdressBook, Pid) -> Pid ! {error, Msg},
  loop(AdressBook);
checkNewAdressBook(NewAdressBook, _, Pid) -> Pid ! ok,
  loop(NewAdressBook).

finding({error, Msg}, AdressBook, Pid) -> checkNewAdressBook({error, Msg}, AdressBook, Pid);
finding(NewAdressBook, AdressBook, Pid) -> Pid ! NewAdressBook,
  loop(AdressBook).
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%Adding%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
addContact(FirstName, LastName) ->
  server ! {addContact, FirstName, LastName, self()},
  receive
    {error, Msg} -> Msg;
    ok -> ok
  end.

addEmail(FirstName, LastName, Email) ->
  server ! {addEmail, FirstName, LastName, Email, self()},
  receive
    {error, Msg} -> Msg;
    ok -> ok
  end.

addPhone(FirstName, LastName, Phone) ->
  server ! {addPhone, FirstName, LastName, Phone, self()},
  receive
    {error, Msg} -> Msg;
    ok -> ok
  end.

addCompany(FirstName, LastName, Company)  ->
  server ! {addCompany, FirstName, LastName, Company, self()},
  receive
    {error, Msg} -> Msg;
    ok -> ok
  end.

addPosition(FirstName, LastName, Position)  ->
  server ! {addPosition, FirstName, LastName, Position, self()},
  receive
    {error, Msg} -> Msg;
    ok -> ok
  end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%Removing%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
removeContact(FirstName, LastName) ->
  server ! {removeContact, FirstName, LastName, self()},
  receive
    {error, Msg} -> Msg;
    ok -> ok
  end.

removeEmail(Email) ->
  server ! {removeEmail, Email, self()},
  receive
    {error, Msg} -> Msg;
    ok -> ok
  end.

removePhone(Phone) ->
  server ! {removePhone, Phone, self()},
  receive
    {error, Msg} -> Msg;
    ok -> ok
  end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%Getting%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
getEmails(FirstName, LastName) ->
  server ! {getEmails, FirstName, LastName, self()},
  receive
    {error, Msg} -> Msg;
    List -> List
  end.

getPhones(FirstName, LastName) ->
  server ! {getPhones, FirstName, LastName, self()},
  receive
    {error, Msg} -> Msg;
    List -> List
  end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%Finding%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
findByEmail(Email) ->
  server ! {findByEmail, Email, self()},
  receive
    {error, Msg} -> Msg;
    List -> List
  end.

findByPhone(Phone) ->
  server ! {findByPhone, Phone, self()},
  receive
    {error, Msg} -> Msg;
    List -> List
  end.

findByCompany(Company) ->
  server ! {findByCompany, Company, self()},
  receive
    {error, Msg} -> Msg;
    List -> List
  end.