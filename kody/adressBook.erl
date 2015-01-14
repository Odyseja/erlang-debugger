%%%-------------------------------------------------------------------
%%%-------------------------------------------------------------------
%%% @author Aleksandra Bańkowska
%%% @copyright (C) 2014, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 21. Nov 2014 10:14 AM
%%%-------------------------------------------------------------------
%%%Zadanie - stworzyć książkę adresową opisaną na stronie internetowej
%%%%%%Zadanie dodatkowe: Dodaj do modułu adressBook możliwość przechowywania informacji o zatrudnieniu (nazwa firmy, stanowisko).
%%%Dodaj możliwość wyszukiwania pracowników według firmy

-module(adressBook).
-author("Aleksandra Bańkowska").

%% API
-export([createAdressBook/0]).
-export([addContact/3]).
-export([addEmail/4]).
-export([addPhone/4]).
-export([removeContact/3]).
-export([removeEmail/2]).
-export([removePhone/2]).
-export([getEmails/3]).
-export([getPhones/3]).
-export([findByEmail/2]).
-export([findByPhone/2]).
-export([findByCompany/2]).
-export([addCompany/4]).
-export([addPosition/4]).


-record(person, {firstName, lastName, phoneNumber=[], emailAdress=[], company, position}).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%Create Book%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
createAdressBook() -> [].
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%Add contact%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
addContact(FirstName, LastName, AdressBook) ->
  case checkIfContactIsUnique(FirstName, LastName, AdressBook) of
    true -> reallyAddContact(FirstName, LastName, AdressBook);
    false -> {error, "Name must be unique"}
  end.

reallyAddContact(FirstName, LastName, AdressBook) -> AdressBook++[#person{firstName=FirstName, lastName=LastName}].

checkIfContactIsUnique(_, _, []) -> true;
checkIfContactIsUnique(FirstName, LastName, [#person{firstName=FirstName, lastName=LastName} | _]) -> false;
checkIfContactIsUnique(FirstName, LastName,[_ | T]) -> checkIfContactIsUnique(FirstName, LastName, T).
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%Add email%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
addEmail(FirstName, LastName, Email, AdressBook) ->
  case checkIfEmailIsUnique(Email, AdressBook) of
    true -> addUniqueEmail(FirstName, LastName, Email, AdressBook);
    false -> {error, "Email must be unique!"}
  end.

checkInsidePersonIfEmailIsUnique(_, [])-> true;
checkInsidePersonIfEmailIsUnique(Email, [Email | _])-> false;
checkInsidePersonIfEmailIsUnique(Email, [_ | T])-> checkInsidePersonIfEmailIsUnique(Email, T).

checkIfEmailIsUnique(_, []) -> true;
checkIfEmailIsUnique(Email, [H | T]) ->
  case checkInsidePersonIfEmailIsUnique(Email, H#person.emailAdress) of
    true -> checkIfEmailIsUnique(Email, T);
    false -> false
  end.

addUniqueEmail(FirstName, LastName, Email, []) -> [#person{firstName = FirstName, lastName = LastName, emailAdress = [Email]}];
addUniqueEmail(FirstName, LastName, Email, [H=#person{firstName = FirstName, lastName = LastName} | T]) -> [#person{firstName = FirstName, lastName = LastName, emailAdress = H#person.emailAdress++[Email], phoneNumber = H#person.phoneNumber, company = H#person.company, position = H#person.position}]++ T;
addUniqueEmail(FirstName, LastName, Email, [H | T]) -> [H]++addUniqueEmail(FirstName, LastName, Email, T).
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%Add phone number%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
addPhone(FirstName, LastName, Phone, AdressBook) ->
  case checkIfPhoneIsUnique(Phone, AdressBook) of
    true -> addUniquePhone(FirstName, LastName, Phone, AdressBook);
    false -> {error, "Phone number must be unique!"}
  end.

checkInsidePersonIfPhoneIsUnique(_, [])-> true;
checkInsidePersonIfPhoneIsUnique(Phone, [Phone | _])-> false;
checkInsidePersonIfPhoneIsUnique(Phone, [_ | T])-> checkInsidePersonIfPhoneIsUnique(Phone, T).

checkIfPhoneIsUnique(_, []) -> true;
checkIfPhoneIsUnique(Phone, [H | T]) ->
  case checkInsidePersonIfPhoneIsUnique(Phone, H#person.phoneNumber) of
    true -> checkIfPhoneIsUnique(Phone, T);
    false -> false
  end.

addUniquePhone(FirstName, LastName, Phone, []) -> [#person{firstName = FirstName, lastName = LastName, phoneNumber = [Phone]}];
addUniquePhone(FirstName, LastName, Phone, [H=#person{firstName = FirstName, lastName = LastName} | T]) -> [#person{firstName = FirstName, lastName = LastName, emailAdress = H#person.emailAdress, phoneNumber = H#person.phoneNumber++[Phone], company = H#person.company, position = H#person.position}]++ T;
addUniquePhone(FirstName, LastName, Phone, [H | T]) -> [H]++addUniquePhone(FirstName, LastName, Phone, T).
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%Remove contact%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
removeContact(_, _, []) -> [];
removeContact(FirstName, LastName, [#person{firstName = FirstName, lastName = LastName} | T]) -> T;
removeContact(FirstName, LastName, [H | T]) -> [H]++removeContact(FirstName, LastName, T).
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%Remove email%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
removeEmail(_, []) -> [];
removeEmail(Email, [H | T]) ->
  case findEmailInPerson(Email, H#person.emailAdress) of
    true -> T++[#person{firstName = H#person.firstName, lastName = H#person.lastName, phoneNumber = H#person.phoneNumber, emailAdress = removeEmailInPerson(Email, H#person.emailAdress), company = H#person.company, position = H#person.position}];
    false -> [H]++removeEmail(Email, T)
  end.

findEmailInPerson(_, []) -> false;
findEmailInPerson(Email, [Email | _]) -> true;
findEmailInPerson(Email, [_ | T]) -> findEmailInPerson(Email, T).

removeEmailInPerson(Email, Numbers) ->
  case Numbers of
    [Email | T] -> T;
    [H | T] -> removeEmailInPerson(Email, T++[H])
  end.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%Remove phone%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
removePhone(_, []) -> [];
removePhone(Phone, [H | T]) ->
  case findPhoneInPerson(Phone, H#person.phoneNumber) of
    true -> T++[#person{firstName = H#person.firstName, lastName = H#person.lastName, phoneNumber = removePhoneInPerson(Phone, H#person.phoneNumber), emailAdress = H#person.emailAdress, company = H#person.company, position = H#person.position}];
    false -> [H]++removePhone(Phone, T)
  end.

findPhoneInPerson(_, []) -> false;
findPhoneInPerson(Phone, [Phone | _]) -> true;
findPhoneInPerson(Phone, [_ | T]) -> findPhoneInPerson(Phone, T).

removePhoneInPerson(Phone, Numbers) ->
  case Numbers of
    [Phone | T] -> T;
    [H | T] -> removePhoneInPerson(Phone, T++[H])
  end.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%Get emails and phone numbers%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
getEmails(_, _, []) -> {error, "No such person"};
getEmails(FirstName, LastName, [H=#person{firstName = FirstName, lastName = LastName} | _]) -> H#person.emailAdress;
getEmails(FirstName, LastName, [_ | T]) -> getEmails(FirstName, LastName, T).

getPhones(_, _, []) -> {error, "No such person"};
getPhones(FirstName, LastName, [H=#person{firstName = FirstName, lastName = LastName} | _]) -> H#person.phoneNumber;
getPhones(FirstName, LastName, [_ | T]) -> getPhones(FirstName, LastName, T).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%Find name and surname%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
findByEmail(_, []) -> {error, "No such email"};
findByEmail(Email, [H | T]) ->
  case findEmailInPerson(Email, H#person.emailAdress) of
    true -> {H#person.firstName, H#person.lastName};
    false -> findByEmail(Email, T)
  end.

findByPhone(_, []) -> {error, "No such phone number"};
findByPhone(Phone, [H | T]) ->
  case findPhoneInPerson(Phone, H#person.phoneNumber) of
    true -> {H#person.firstName, H#person.lastName};
    false -> findByPhone(Phone, T)
  end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%Add company and position%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
addCompany(FirstName, LastName, Company, []) -> [#person{firstName = FirstName, lastName = LastName, company = Company}];
addCompany(FirstName, LastName, Company, [H=#person{firstName = FirstName, lastName = LastName} | T]) -> [#person{firstName = FirstName, lastName = LastName, phoneNumber = H#person.phoneNumber, emailAdress = H#person.emailAdress, company = Company, position = H#person.position}]++T;
addCompany(FirstName, LastName, Company, [H | T]) -> [H]++addCompany(FirstName, LastName, Company, T).

addPosition(FirstName, LastName, Position, []) -> [#person{firstName = FirstName, lastName = LastName, position =  Position}];
addPosition(FirstName, LastName, Position, [H=#person{firstName = FirstName, lastName = LastName} | T]) -> [#person{firstName = FirstName, lastName = LastName, phoneNumber = H#person.phoneNumber, emailAdress = H#person.emailAdress, company = H#person.company, position = Position}]++T;
addPosition(FirstName, LastName, Position, [H | T]) -> [H]++addPosition(FirstName, LastName, Position, T).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%Find worker of the company%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
findByCompany(_, []) -> [];
findByCompany(Company, [H | T]) ->
  case H#person.company of
    Company -> findByCompany(Company, T)++[{H#person.firstName, H#person.lastName, H#person.position}];
    _ -> findByCompany(Company, T)
  end.