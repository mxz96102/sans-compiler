|-<ST>:start|0
|  |-<DECL>:float|1
|  |  |-<ASSIGN>:<-|1
|  |  |  |--<VAR>:a|1
|  |  |  |--<FLOAT>:0.1|1
|  |-<DECL>:float|2
|  |  |-<ASSIGN>:<-|2
|  |  |  |--<VAR>:b|2
|  |  |  |-<STAR>:*|2
|  |  |  |  |-<PAR>:()|2
|  |  |  |  |  |-<PLUS>:+|2
|  |  |  |  |  |  |--<FLOAT>:2.0|2
|  |  |  |  |  |  |--<FLOAT>:3.0|2
|  |  |  |  |--<FLOAT>:5.0|2
|  |-<DECL>:float|3
|  |  |-<ASSIGN>:<-|3
|  |  |  |--<VAR>:d|3
|  |  |  |--<FLOAT>:1.2|3
|  |-<DECL>:char|4
|  |  |-<ASSIGN>:<-|4
|  |  |  |--<VAR>:e|4
|  |  |  |--<CHAR>:.|4
|  |-<DECL>:string|6
|  |  |-<ASSIGN>:<-|6
|  |  |  |--<VAR>:str|6
|  |  |  |-<PLUS>:+|6
|  |  |  |  |--<STRING>:I|6
|  |  |  |  |-<PLUS>:+|6
|  |  |  |  |  |--<STRING>:was|6
|  |  |  |  |  |--<STRING>:silly|6
|  |-<IFEXPR>:if|8
|  |  |-<BTH>:>|8
|  |  |  |--<VAR>:d|8
|  |  |  |--<FLOAT>:0.1|8
|  |  |-<THEN>:then|8
|  |  |  |-<ASSIGN>:<-|9
|  |  |  |  |--<VAR>:d|9
|  |  |  |  |--<FLOAT>:2.1|9
|  |-<IFEXPR>:if|14
|  |  |-<BTH>:>|14
|  |  |  |--<VAR>:a|14
|  |  |  |--<INT>:100|14
|  |  |-<THEN>:then|14
|  |  |  |-<DECL>:int|15
|  |  |  |  |-<ASSIGN>:<-|15
|  |  |  |  |  |--<VAR>:c|15
|  |  |  |  |  |--<INT>:0|15
|  |  |  |-<WEXPR>:while|16
|  |  |  |  |-<LTH>:<|16
|  |  |  |  |  |--<VAR>:c|16
|  |  |  |  |  |--<INT>:100|16
|  |  |  |  |-<DO>:do|16
|  |  |  |  |  |-<PLUSE>:+=|17
|  |  |  |  |  |  |--<VAR>:c|17
|  |  |  |  |  |  |--<INT>:1|17
|  |  |  |-<PP>:++|19
|  |  |  |  |--<VAR>:b|19
|  |-<ELSE>:else|20
|  |  |-<IFEXPR>:if|20
|  |  |  |-<LTH>:<|20
|  |  |  |  |--<VAR>:a|20
|  |  |  |  |--<INT>:400|20
|  |  |  |-<THEN>:then|20
|  |  |  |  |-<ASSIGN>:<-|21
|  |  |  |  |  |--<VAR>:b|21
|  |  |  |  |  |--<INT>:200|21
|  |  |  |  |-<ASSIGN>:<-|22
|  |  |  |  |  |--<VAR>:a|22
|  |  |  |  |  |-<PLUS>:+|22
|  |  |  |  |  |  |--<VAR>:a|22
|  |  |  |  |  |  |--<CHAR>:2|22
error type float char at line 22
error type float error at line 22

name	type	length	address	declare	scope	use
a	float	4	000000H	1	1	22,20,14,
e	char	1	00000CH	4	1	
b	float	4	000004H	2	1	21,19,
d	float	4	000008H	3	1	9,8,
str	string	9	00000DH	6	1	
c	int	2	000016H	15	1	17,16,
