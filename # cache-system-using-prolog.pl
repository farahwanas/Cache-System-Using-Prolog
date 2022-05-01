convertBinToDec(Bin,Dec):-
   binhelper(Bin,0,Dec).
binhelper(0,_,0).
binhelper(Bin,I,R):-
   Bin \=0,
   M is Bin mod 10,
   Dec1 is 2**I,
   Dec2 is Dec1*M,
   Bin1 is Bin//10,
   I1 is I+1,
   binhelper(Bin1,I1,Dec3),
   R is Dec3 + Dec2.

replaceIthItem(X,[H|T],N,R):-
	replaceIthItemhelper(X,[H|T],N,0,R).
replaceIthItemhelper(_,[],_,_,[]).

replaceIthItemhelper(X,[H|T],N,Acc,[H|R]):- Acc\=N,
                                            Acc1 is Acc+1,
                                            replaceIthItemhelper(X,T,N,Acc1,R).

  

replaceIthItemhelper(X,[_|T],N,Acc,[X|R]):- Acc=N,
                                            Acc1 is Acc+1,
                                            replaceIthItemhelper(X,T,N,Acc1,R).                    
		
		
splitEvery(N,L,R):-
		splitHelper(N,L,1,[],R).

splitHelper(_,[],_,Acc,Acc).
splitHelper(N,[H|T],I,Acc,R):-
		N\=I,
		append(Acc,[H],Nacc),
		I1 is I+1,
		splitHelper(N,T,I1,Nacc,R).
splitHelper(N,[H|T],I,Acc,[Nacc|R]):-
		I=N,
		append(Acc,[H],Nacc),
		splitHelper(N,T,1,[],R).
				
  
logBase2(Num,Res):-
		logHelper(Num,0,Res).
logHelper(1,I,I).
logHelper(Num,I,Res):-
		Num \=0,
		Num1 is Num //2,
		I1 is I+1,
		logHelper(Num1,I1,Res).

/*getNumBits(NumOfSets,Type,Cache,BitsNum)*/
getNumBits(_,fullyAssoc,_,0).
getNumBits(NumOfSets,setAssoc,_,BitsNum):-
                 logBase2(NumOfSets,BitsNum).
getNumBits(_,directMap,L,BitsNum):-
              length(L,Len),
              logBase2(Len,BitsNum).	
fillZeros(R,0,R).	
fillZeros(String,N,R):-
		N\=0,
		N1 is N-1,
	       string_concat("0",String,R1),
		fillZeros(R1,N1,R).
convertAddress(Bin,BitsNum,Tag,Idx,directMap):-	
        X is 10**BitsNum,
	Idx is Bin mod X,
	Tag is Bin//X.

getDataFromCache(StringAddress,Cache,Data,0,directMap,BitsNum):-
	        atom_number(StringAddress, Address),
  		convertAddress(Address,BitsNum,Tag,Idx,directMap),
                convertBinToDec(Idx,Dec),
		nth0(Dec,Cache,Result),
	        Result =item(tag(Tag2),data(Data),1,0),
	        atom_number(Tag2,Tag).
convertAddress(Bin,SetsNum,Tag,Idx,setAssoc):-
          getNumBits(SetsNum,setAssoc,_,BitsNum),
          Idx is Bin mod 10**BitsNum,
          Tag is Bin//10**BitsNum.	
getDataFromCache(StringAddress,Cache,Data,HopsNum,setAssoc,SetsNum):-
            length(Cache,L1),
			Numofsplits is L1//SetsNum,
            splitEvery(Numofsplits,Cache,SubL),
			atom_number(StringAddress,AddressNum),
			convertAddress(AddressNum,SetsNum,Tag,Idx,setAssoc),
			convertBinToDec(Idx,Dec),
			nth0(Dec,SubL,L2),
			getDatafromCacheHelper(L2,Tag,setAssoc,Data,0,HopsNum).

		

getDatafromCacheHelper([H|_],Tag,setAssoc,Data,Acc,Acc):-
          H=item(tag(Tag1),data(Data),1,_),
		  atom_number(Tag1,Tag).
		 
		   
getDatafromCacheHelper([H|T],Tag,setAssoc,Data,Acc,HopsNum):-
          H=item(_,_,0,_),
		  Acc1 is Acc+1,
		  getDatafromCacheHelper(T,Tag,setAssoc,Data,Acc1,HopsNum).
         
getDatafromCacheHelper([H|T],Tag,setAssoc,Data,Acc,HopsNum):-
        H=item(tag(Tag2),_,1,_),
		atom_number(Tag2,Tag1),
        Tag1\=Tag,
		Acc1 is Acc +1,
		getDatafromCacheHelper(T,Tag,setAssoc,Data,Acc1,HopsNum).

convertAddress(Bin,0,Tag,0,fullyAssoc):-
    Tag is Bin/ 10**0.

getDataFromCache(StringAddress,Cache,Data,HopsNum,fullyAssoc,BitsNum):-
             atom_number(StringAddress,Address),
             getDataFromCacheHelper(Address,Cache,Data,HopsNum,fullyAssoc,BitsNum,0).
getDataFromCacheHelper(Address,[H|T],Data,HopsNum,fullyAssoc,BitsNum,Acc):-
            H=item(_,_,0,_),
            Acc1 is Acc+1,
            getDataFromCacheHelper(Address,T,Data,HopsNum,fullyAssoc,BitsNum,Acc1).

getDataFromCacheHelper(Address,[H|T],Data,HopsNum,fullyAssoc,BitsNum,Acc):-
           H=item(tag(Tag2),_,1,_),
           atom_number(Tag2,Tag),
           Tag\=Address,
           Acc1 is Acc+1,
           getDataFromCacheHelper(Address,T,Data,HopsNum,fullyAssoc,BitsNum,Acc1).

getDataFromCacheHelper(Address,[H|T],Data,Acc,fullyAssoc,BitsNum,Acc):-
            H=item(tag(Tag1),data(Data),1,_),
            atom_number(Tag1,Address).


replaceInCache(Tag,Idx,Mem,OldCache,NewCache,ItemData, directMap,BitsNum):-
getNumBits(_,directMap, OldCache,IdxBits),
atom_length(Idx,IdxLength),
Y is IdxBits-IdxLength,
fillZeros(Idx,Y,Hpp),
string_concat(Tag,Hpp,AddString),
atom_number(AddString,Add),
convertBinToDec(Add,Address),
nth0(Address,Mem,ItemData),
atom_length(Tag,TagLength),
X is 6-IdxBits-TagLength,
fillZeros(Tag,X,New_tag),
convertBinToDec(Idx,IDX),
replaceIthItem(item(tag(New_tag),data(ItemData),1,0), OldCache, IDX, NewCache).



replaceInCache(Tag,Idx,Mem,OldCache,NewCache,ItemData, setAssoc,SetsNum):-
getNumBits(SetsNum,setAssoc,_,IdxBits),
atom_length(Idx,IdxLength),
Y is IdxBits-IdxLength,
fillZeros(Idx,Y,Hpp),
string_concat(Tag,Idx,AddString),
atom_number(AddString,Add),
convertBinToDec(Add,Address),
nth0(Address,Mem,ItemData),
atom_length(Tag,TagLength),
X is 6-IdxBits-TagLength,
fillZeros(Tag,X,New_tag),
length(OldCache, OldCacheLength),
SetLength is OldCacheLength/SetsNum,
splitEvery(SetLength, OldCache,R),
convertBinToDec(Idx,IDX),
nth0(IDX,R,Listgowaelset),
max_list(Listgowaelset,G),
nth0(P,Listgowaelset,G),
order(Listgowaelset,TempListgowaelset),
replaceIthItem(item(tag(New_tag),data(ItemData),1,0),TempListgowaelset,P,TempList),
replaceIthItem(TempList,R,IDX,NewCacheTemp),
flatten(NewCacheTemp,NewCache).


/*replaceInCache(Tag,Idx,Mem,OldCache,NewCache,ItemData,fullyAssoc, BitsNum):-
convertBinToDec(Tag,Address),
nth0(Address,Mem,ItemData),
atom_length(Tag,TagLength),
X is 6-TagLength,
fillZeros(Tag,X,New_tag),
max_list(OldCache,G),
nth0(R,OldCache,G),
order(OldCache,TempCache),
replaceIthItem(item(tag(New_tag),data(ItemData),1,0),TempCache,R, NewCache).
*/
replaceInCache(Tag,Idx,Mem,OldCache,NewCache,ItemData,fullyAssoc, BitsNum):-
atom_number(StringTag,Tag),
string_length(StringTag,LengthElTag),
X is 6-LengthElTag,
fillZeros(StringTag,X,New_tag),
convertBinToDec(Tag,Address),
nth0(Address,Mem,ItemData),
max_list(OldCache,G),
nth0(R,OldCache,G),
order(OldCache,TempCache),
replaceIthItem(item(tag(New_tag),data(ItemData),1,0),TempCache,R,NewCache).



max_list(L,R):-max(L,item(_,_,1,-1),R).

max([item(A,B,0,C)|_],_,item(A,B,0,C)).
max([],K,K).
max([item(A,B,1,C)|T],item(_,_,1,X),Max):-C>X,
					  max(T,item(A,B,1,C),Max).
max([item(_,_,1,C)|T],item(A,B,1,X),Max):-C=<X,
					  max(T,item(A,B,1,X),Max).

/*
order([],[]).
order([item(A,B,1,X)|T],[item(A,B,1,C)|T1]):- C is X+1,
			  	             order(T,T1).
order([item(A,B,0,X)|T],[item(A,B,0,X)|T1]):- order(T,T1).
*/
order([],[]).
order([item(tag(X),data(Y),1,P)|T],[Z|NewCache]):-
            order1helper(item(tag(X),data(Y),1,P),Z),
            order(T,NewCache).
order([item(tag(X),data(Y),0,P)|T],[item(tag(X),data(Y),0,P)|NewCache]):-
    order(T,NewCache).
order1helper(item(X,Y,1,P),item(X,Y,1,P1)):-
         P1 is P+1.

getData(StringAddress,OldCache,Mem,NewCache,Data,HopsNum,Type,BitsNum,hit):-
getDataFromCache(StringAddress,OldCache,Data,HopsNum,Type,BitsNum),
NewCache = OldCache.
getData(StringAddress,OldCache,Mem,NewCache,Data,HopsNum,Type,BitsNum,miss):-
         \+getDataFromCache(StringAddress,OldCache,Data,HopsNum,Type,BitsNum),
         atom_number(StringAddress,Address),
         convertAddress(Address,BitsNum,Tag,Idx,Type),
         replaceInCache(Tag,Idx,Mem,OldCache,NewCache,Data,Type,BitsNum).

runProgram([],OldCache,_,OldCache,[],[],Type,_). 
runProgram([Address|AdressList],OldCache,Mem,FinalCache,
[Data|OutputDataList],[Status|StatusList],Type,NumOfSets):- getNumBits(NumOfSets,Type,OldCache,BitsNum),
(Type = setAssoc, Num = NumOfSets; Type \= setAssoc, Num = BitsNum), getData(Address,OldCache,Mem,NewCache,Data,HopsNum,Type,Num,Status),
runProgram(AdressList,NewCache,Mem,FinalCache,OutputDataList,StatusList,Type,NumOfSets).
