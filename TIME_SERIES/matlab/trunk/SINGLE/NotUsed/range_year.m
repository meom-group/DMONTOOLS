function [starty endy]=range_year(filename,nhead)
%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%% PLOTTING ROUTINE FOR TRANSPORTS
%%%%%%%%%%%%%%%%%%%%%%%
eval(['tmp=load(''' filename ''');']) ;
starty  = tmp(nhead+1  ,1) ; 
endy    = tmp(end,1) ;
clear tmp

