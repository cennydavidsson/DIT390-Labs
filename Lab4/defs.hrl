% This record defines the structure of the 
% client process. 
% 
% It contains the following fields: 
%
% gui: it stores the name (or Pid) of the GUI process.
%
-record(cl_st, {gui,nick,server,machine,channels=[]}).
    
% This record defines the structure of the 
% server process. 
% 
-record(server_st, {name,clients=[],channels=[]}).

% This record defines the structure of the 
% channel process. 
% 
-record(channel_st, {name,clients=[]}).