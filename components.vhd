library IEEE;
use IEEE.std_logic_1164.all;

entity mux24 is
	port(a, b: in  std_logic_vector(23 downto 0);
		  sel: in std_logic;
			c: out std_logic_vector(23 downto 0));
end mux24;

architecture behav of mux24 is
begin
	process (a, b, sel)
	begin
		if sel = '1' then
			c <= a;
		else
			c <= b;
		end if;
	end process;
end behav;

library IEEE;
use IEEE.std_logic_1164.all;

entity mux8 is
	port(a, b: in std_logic_vector(8 downto 0);
	     sel: in std_logic;
		 c: out std_logic_vector(8 downto 0));

end mux8;

architecture behav_mux8 of mux8 is
begin

	process (a, b, sel)	
	begin
		if sel = '0' then
			c <= a;
		else
			c <= b;
		end if;
	end process;
	
end behav_mux8;

library IEEE;
use IEEE.std_logic_1164.all;

entity mux1 is
	port (a, b: in std_logic;
		  sel: in std_logic;
		  c: out std_logic);
end mux1;

architecture behav_mux1 of mux1 is
begin
	process(a, b, sel)
	begin
		if sel = '0' then
			c <= a;
		else 
			c <= b;
		end if;
	end process;
end behav_mux1;

library IEEE;
use IEEE.std_logic_1164.all;
use IEEE.numeric_std.all;
entity sub_exp is
	port(a, b:	in  std_logic_vector(8 downto 0);
	     result: 	out std_logic_vector (8 DOWNTO 0));
end sub_exp;

architecture functional of sub_exp is
begin
  result <= std_logic_vector(unsigned(a)+unsigned(unsigned(not b)+1));
end architecture functional;


library IEEE;
use IEEE.std_logic_1164.all;
use IEEE.numeric_std.all;
entity compl2_24 is
	port(a : in std_logic_vector (23 downto 0);
		 enable: in std_logic;
		 b : out std_logic_vector (24 downto 0));
end compl2_24;

architecture arc of compl2_24 is
begin
	process (a, enable)
	variable tmp : std_logic_vector(24 downto 0);
	begin
		tmp := '0' & a;
		if enable='1' then
			b <= std_logic_vector(unsigned(unsigned(not tmp)+1));
		else
			b <= tmp;
		end if;
	end process;
end architecture arc;

library IEEE;
use IEEE.std_logic_1164.all;
use IEEE.numeric_std.all;
entity compl2_25 is
	port(a : in std_logic_vector (24 downto 0);
		 enable: in std_logic;
		 b : out std_logic_vector (24 downto 0));
end compl2_25;

architecture arc of compl2_25 is
begin
	process (a, enable)
	begin
		if enable='1' then
			b <= std_logic_vector(unsigned(unsigned(not a)+1));
		else
			b <= a;
		end if;
	end process;
end architecture arc;


library IEEE;
use IEEE.std_logic_1164.all;
use IEEE.numeric_std.all;
entity compl2_8 is
	port(a : in std_logic_vector (8 downto 0);
		 enable: in std_logic;
		 b : out std_logic_vector (8 downto 0));
end compl2_8;

architecture arc of compl2_8 is
begin
	process (a, enable)
	begin
		if enable='1' then
			b <= std_logic_vector(unsigned(unsigned(not a)+1));
		else
			b <= a;
		end if;
	end process;

end architecture arc;

library IEEE;
use IEEE.std_logic_1164.all;
use IEEE.numeric_std.all;
use ieee.std_logic_unsigned.all;

entity sum is
	port (   a: in std_logic_vector(24 downto 0);
			 b: in std_logic_vector(24 downto 0);
			 r: out std_logic_vector(24 downto 0);
			ow: out std_logic);
end sum;

architecture func of sum is
	signal result: std_logic_vector(25 downto 0);
begin
	process(a,b)	
	variable tmp11, tmp23, tmp3 : integer;
	begin
		
		tmp11 := to_integer(unsigned(a));
		tmp23 := to_integer(unsigned(b));
		tmp3 := (tmp11+tmp23);
		result <= std_logic_vector(to_unsigned(tmp3, 26));
	end process;
	r <= result(24 downto 0);
	ow <= result(25);
end func;




library IEEE;
use IEEE.std_logic_1164.all;
use IEEE.numeric_std.all;
entity shift_mantissa is
	port (a		: in std_logic_vector(24 downto 0);
	      n		: in std_logic_vector(8 downto 0);
	      g, r, s	: out std_logic;
	      result	: out std_logic_vector(24 downto 0));

	signal tmp, tmp2: std_logic_vector(49 downto 0);
	signal resto: std_logic_vector(24 downto 0) := (others => '0');
end shift_mantissa;

architecture sh_mant of shift_mantissa is
begin
	tmp <= a & "0000000000000000000000000";
	tmp2 <= to_stdlogicvector(to_bitvector(tmp) sra to_integer(unsigned(n)));
--	resto((to_integer(unsigned(n))-1) downto 0) <= a((to_integer(unsigned(n))-1) downto 0);
	resto <= tmp2(24 downto 0);
   	result <= tmp2(49 downto 25);	
--	resto <= to_stdlogicvector((to_bitvector(a) sll (24-(to_integer(unsigned(n))))));

	g <= resto(24);
	r <= resto(23);

	s <= resto(22) or
		 resto(21) or
		 resto(20) or
		 resto(19) or
		 resto(18) or
		 resto(17) or
		 resto(16) or
		 resto(15) or
		 resto(14) or
		 resto(13) or
		 resto(12) or
		 resto(11) or
		 resto(10) or
		 resto(9) or
		 resto(8) or
		 resto(7) or
		 resto(6) or
		 resto(5) or
		 resto(4) or
		 resto(3) or
		 resto(2) or
		 resto(1) or
		 resto(0);

--	tmp <= a;
--	tmp2 <= to_stdlogicvector(to_bitvector(tmp) sra to_integer(unsigned(n)));
--  result <= tmp2(23 downto 0);

--	process (tmp, n)
--	begin	
--		count <= n;
--		tmp <= a;
--		while (unsigned(count)>0) loop
--			tmp <= '0' & tmp( 23 downto 1);
--			count <= std_logic_vector(unsigned(count)-1);
--		end loop;
--		result <= tmp;
--
--
--	end process;

end sh_mant;


library IEEE;
use IEEE.std_logic_1164.all;
use IEEE.numeric_std.all;
use ieee.std_logic_unsigned.all;
entity shift_sum_left is
	port(a	: in std_logic_vector(24 downto 0);
		 en : in std_logic;
	     g	: in std_logic;
	     result	: out std_logic_vector(24 downto 0);
	     count	: out std_logic_vector(7 downto 0));
	signal tmp: std_logic_vector(7 downto 0);
	signal rtmp: std_logic_vector(24 downto 0);
	signal m23, m22, m21, m20, m19, m18, m17, m16, m15, m14, m13, m12, m11,
   		   m10, m9, m8, m7, m6, m5, m4, m3, m2, m1, m0: std_logic;
	signal shamt, shamt_tmp: std_logic_vector(4 downto 0);
end shift_sum_left;

architecture ssl of shift_sum_left is
begin
--	m0 <= a(23);
--    m1 <= (not(m0))	and a(22);
--    m2 <= (not(m1)) and a(21);
--	m3 <= (not(m2)) and a(20);
--	m4 <= (not(m3)) and a(19);
--	m5 <= (not(m4)) and a(18);
--	m6 <= (not(m5)) and a(17);
--	m7 <= (not(m6)) and a(16);
--	m8 <= (not(m7)) and a(15);
--	m9 <= (not(m8)) and a(14);
--	m10 <= (not(m9)) and a(13);
--	m11 <= (not(m10)) and a(12);
--	m12 <= (not(m11)) and a(11);
--	m13 <= (not(m12)) and a(10);
--	m14 <= (not(m13)) and a(9);
--	m15 <= (not(m14)) and a(8);
--	m16 <= (not(m15)) and a(7);
--	m17 <= (not(m16)) and a(6);
--	m18 <= (not(m17)) and a(5);
--	m19 <= (not(m18)) and a(4);
--	m20 <= (not(m19)) and a(3);
--	m21 <= (not(m20)) and a(2);
--	m22 <= (not(m21)) and a(1);
--	m23 <= (not(m22)) and a(0);

--	m23 <= a(23);
--  m22 <= not(m23)	and a(22);
--  m21 <= (not(m22)) and a(21);
--	m20 <= (not(m21)) and a(20);
--	m19 <= (not(m20)) and a(19);
--	m18 <= (not(m19)) and a(18);
--	m17 <= (not(m18)) and a(17);
--	m16 <= (not(m17)) and a(16);
--	m15 <= (not(m16)) and a(15);
--	m14 <= (not(m15)) and a(14);
--	m13 <= (not(m14)) and a(13);
--	m12 <= (not(m13)) and a(12);
--	m11 <= (not(m12)) and a(11);
--	m10 <= (not(m11)) and a(10);
--	m9 <= (not(m10)) and a(9);
--	m8 <= (not(m9)) and a(8);
--	m7 <= (not(m8)) and a(7);
--	m6 <= (not(m7)) and a(6);
--	m5 <= (not(m6)) and a(5);
--	m4 <= (not(m5)) and a(4);
--	m3 <= (not(m4)) and a(3);
--	m2 <= (not(m3)) and a(2);
--	m1 <= (not(m2)) and a(1);
--	m0 <= (not(m1)) and a(0);

--	shamt(0) <= m23 or m20 or m18 or m16 or m14 or m12 or m10 or m8 or m6 or m4 or m2 or m0;
--	shamt(1) <= m23 or m22 or m19 or m18 or m15 or m14 or m11 or m10 or m7 or m6 or m3 or m1;
--	shamt(2) <= m23 or m22 or m21 or m20 or m15 or m14 or m13 or m12 or m7 or m6 or m5 or m4;
--	shamt(3) <= m15 or m14 or m13 or m12 or m11 or m10 or m9 or m8;
--	shamt(4) <= m23 or m22 or m21 or m20 or m19 or m18 or m17 or m16;

	P1: process(a)
	variable n : std_logic_vector(4 downto 0);
	begin
	
		if a(23) = '1' then n := "00000";
	   	elsif a(22) = '1' then n := "00001";	
	   	elsif a(21) = '1' then n := "00010";	
	   	elsif a(20) = '1' then n := "00011";	
	   	elsif a(19) = '1' then n := "00100";	
	   	elsif a(18) = '1' then n := "00101";	
	   	elsif a(17) = '1' then n := "00110";	
	   	elsif a(16) = '1' then n := "00111";	
	   	elsif a(15) = '1' then n := "01000";	
	   	elsif a(14) = '1' then n := "01001";	
	   	elsif a(13) = '1' then n := "01010";	
	   	elsif a(12) = '1' then n := "01011";	
	   	elsif a(11) = '1' then n := "01100";	
	   	elsif a(10) = '1' then n := "01101";	
	   	elsif a(9) = '1' then n :=  "01110";	
	   	elsif a(8) = '1' then n :=  "01111";	
	   	elsif a(7) = '1' then n :=  "10000";	
	   	elsif a(6) = '1' then n :=  "10001";	
	   	elsif a(5) = '1' then n :=  "10010";	
	   	elsif a(4) = '1' then n :=  "10011";	
	   	elsif a(3) = '1' then n :=  "10100";	
	   	elsif a(2) = '1' then n :=  "10101";	
	   	elsif a(1) = '1' then n :=  "10110";	
	   	elsif a(0) = '1' then n :=  "10111";	
		else n := "11000";	
	   	--elsif a(0) = '1' then n :=  "11000";	
		end if;

		shamt_tmp <= n;

	end process P1;

	shamt <= shamt_tmp(4 downto 0);

	P2: process (a, shamt, en)
	variable i_1, i_2, i_4, i_8, i_16 : std_logic_vector(25 downto 0);
	variable tmpg : std_logic_vector(25 downto 0);
	begin

		tmpg := a(24 downto 0) & g;
		if shamt(0) = '1' then i_1  := tmpg(24 downto 0) & b"0";
		else                   i_1  := tmpg;
		end if; 
		if shamt(1) = '1' then i_2  := i_1(23 downto 0) & b"00";
		else                   i_2  := i_1;
		end if; 
		if shamt(2) = '1' then i_4  := i_2(21 downto 0) & b"0000";
		else                   i_4  := i_2;
		end if; 
		if shamt(3) = '1' then i_8  := i_4(17 downto 0) & b"00000000";
		else                   i_8  := i_4;
		end if; 
		if shamt(4) = '1' then i_16 := i_8(9 downto 0) & b"0000000000000000";
		else                   i_16 := i_8;
		end if; 


		if en='1' then
			result <= i_16(25 downto 1);
		else
			result <= a(24 downto 0);
		end if;

	end process P2;
	
	count <= "000" & shamt(4 downto 0);

--	process (a)
--	begin
--		tmp <= "00000000";
--		rtmp <= a;
--		if (rtmp(23)/='1') then
--			rtmp<= rtmp(21 downto 0) & g;
--			tmp <= std_logic_vector(unsigned(tmp)+1);
--		end if;
--		while (rtmp(23)/='1') loop
--			rtmp<= rtmp(21 downto 0) & '0';
--			tmp <= std_logic_vector(unsigned(tmp)+1);
--		end loop;
--		count <= tmp;
--		result <= rtmp;
--	end process;
end ssl;


library IEEE;
use IEEE.std_logic_1164.all;
use IEEE.numeric_std.all;
entity pandora is
	port( lst_sign: in std_logic;
		  r: in std_logic;
		  s: in std_logic;
		  g: in std_logic;
		  out_and: in std_logic;
		  count: in std_logic_vector(7 downto 0);
		  rl: out std_logic;
		  sl: out std_logic );
end pandora;

architecture pand of pandora is
begin
	process(lst_sign, r, s, g, out_and, count)
	begin
		if (out_and = '1') then
			rl <= lst_sign;
			sl <= g or s or r;
		elsif (unsigned(count) = 0) then
			rl <= g;
			sl <= r or s;
		elsif (unsigned(count) = 1) then
			rl <= r;
			sl <= s;
		elsif (unsigned(count) > 1) then
			rl <= '0';
			sl <= '0';
		end if;	
	end process;
end pand;

library IEEE;
use IEEE.std_logic_1164.all;
use IEEE.numeric_std.all;
entity shift_one_right is
	port (a		: in std_logic_vector(24 downto 0);
	      enable	: in std_logic;
	      result	: out std_logic_vector(24 downto 0));
end shift_one_right;

architecture sor of shift_one_right is
begin
	process (a, enable)
	begin
		if (enable='1') then
			result <= '1' & a(24 downto 1);
		else
			result <= a;
		end if;
	end process;
end sor;


library IEEE;
use IEEE.std_logic_1164.all;
use IEEE.numeric_std.all;
entity round is
	port (mst_sign	: in std_logic;
		  round		: in std_logic;
	      sticky	: in std_logic;
	      sign		: in std_logic;
	      result	: out std_logic);

	signal tmp: std_logic;

end round;

architecture rd of round is

begin
	process (mst_sign, round, sticky, sign)
	begin
		if ((round = '1') and ( (mst_sign = '1') or (sticky = '1')) ) then
			tmp <= '1';
		else
			tmp <= '0';
		end if;
	end process;
	result <= tmp;
end rd;


library IEEE;
use IEEE.std_logic_1164.all;
use IEEE.numeric_std.all;
entity fix_exp is
	port (exp		: in std_logic_vector(8 downto 0);
		  out_and	: in std_logic_vector(7 downto 0);
	      vaium		: in std_logic_vector(7 downto 0);
	      count		: in std_logic_vector(7 downto 0);
	      result	: out std_logic_vector(7 downto 0));

	signal tmp: std_logic_vector(8 downto 0);
	signal tmp2: std_logic_vector(8 downto 0);
	signal tmp3: std_logic_vector(8 downto 0);

end fix_exp;

architecture rd of fix_exp is
begin
	process (exp, out_and, vaium, count, tmp, tmp2, tmp3)
	begin
		-- TODO: oveflow!
		tmp <= std_logic_vector(unsigned(out_and) + unsigned(vaium) + unsigned(exp));
  		tmp2 <= std_logic_vector(unsigned(tmp)+unsigned(unsigned(not count)+1));
--		tmp2 <= std_logic_vector((unsigned(tmp) - unsigned(count)));
		if ( out_and(0) = '0') then
			tmp3 <= tmp2;
		else
			tmp3 <= tmp;
		end if;
	end process;
	result <= tmp3(7 downto 0);
end rd;

library IEEE;
use IEEE.std_logic_1164.all;
use IEEE.numeric_std.all;
entity marido_de_aluguel is
	port(clock: in std_logic;
	     sinal1,
	     sinal2: in std_logic;
	     exp1,
	     exp2: in std_logic_vector(7 downto 0);
	     mantissa1,
	     mantissa2: in std_logic_vector(22 downto 0);
	     sinal_mux: in std_logic;
	     EX1_Inf,
	     EX1_NaN,
	     EX1_sinal: out std_logic);
end marido_de_aluguel;

architecture mda of marido_de_aluguel is
signal sinaltmp, nan, inf: std_logic;
begin

	process (sinal_mux, sinal1, sinal2, exp1, exp2, mantissa1, mantissa2)
	variable and_exp1, and_exp2, or_exp1, or_exp2, or_mantissa1, or_mantissa2: std_logic;
	begin
		and_exp1 := exp1(7) and exp1(6) and exp1(5) and exp1(4) and exp1(3) and exp1(2) and exp1(1) and exp1(0); -- exp1=="11111111"
		and_exp2 := exp2(7) and exp2(6) and exp2(5) and exp2(4) and exp2(3) and exp2(2) and exp2(1) and exp2(0); -- exp2=="11111111"
		
		or_exp1 := exp1(7) or exp1(6) or exp1(5) or exp1(4) or exp1(3) or exp1(2) or exp1(1) or exp1(0); 	 -- exp1!="00000000"
		or_exp2 := exp2(7) or exp2(6) or exp2(5) or exp2(4) or exp2(3) or exp2(2) or exp2(1) or exp2(0);	 -- exp2!="00000000"

		or_mantissa1 := mantissa1(22) or mantissa1(21) or mantissa1(20) or mantissa1(19)
		or mantissa1(18) or mantissa1(17) or mantissa1(16) or mantissa1(15) or mantissa1(14) or mantissa1(13) or mantissa1(12)
		or mantissa1(11) or mantissa1(10) or mantissa1(9) or mantissa1(8) or mantissa1(7) or mantissa1(6) or mantissa1(5)
		or mantissa1(4) or mantissa1(3) or mantissa1(2) or mantissa1(1) or mantissa1(0);			 -- mantissa1!="00000000000000000000000"
			
		or_mantissa2 := mantissa2(22) or mantissa2(21) or mantissa2(20) or mantissa2(19)
		or mantissa2(18) or mantissa2(17) or mantissa2(16) or mantissa2(15) or mantissa2(14) or mantissa2(13) or mantissa2(12)
		or mantissa2(11) or mantissa2(10) or mantissa2(9) or mantissa2(8) or mantissa2(7) or mantissa2(6) or mantissa2(5)
		or mantissa2(4) or mantissa2(3) or mantissa2(2) or mantissa2(1) or mantissa2(0);			 -- mantissa2!="00000000000000000000000" 

		if (and_exp1='1') then					 -- operando1=NaN ou Infinito
			if (or_mantissa1='1') then			 -- operando1==NaN
				nan <= '1';
				inf <= '0';
				if (and_exp2='1') then			 -- operando2=NaN ou Infinito
					if (or_mantissa2='1') then	 -- operando2==NaN
						sinaltmp<= sinal1 and sinal2;
					else				 -- operando2==Infinito
						sinaltmp<= sinal1;
					end if;
				else					 -- operando2=Num ou 0
					sinaltmp<= sinal1;
				end if;
			else						 -- operando1=Infinito
				if (and_exp2='1') then			 -- operando2=NaN ou Infinito
					if (or_mantissa2='1') then	 -- operando2==NaN
						nan <= '1';
						inf <= '0';
						sinaltmp<= sinal2;
					else				 -- operando2==Infinito
						sinaltmp<= sinal1 and sinal2;
						nan <= sinal1 xor sinal2;
						inf <= sinal1 xnor sinal2;
						
					end if;
				else					 -- operando2=Num ou 0
					sinaltmp<= sinal1;
					nan <= '0';
					inf <= '1';
			 	end if;
			end if;
		else							 -- operando1=Num ou 0
			if ((or_mantissa1='0') and (or_exp1='0')) then	 -- operando1==0
				if (and_exp2='1') then			 -- operando2=NaN ou Infinito
					sinaltmp<= sinal2;
					if (or_mantissa2 = '1') then	 -- operando2=NaN
						nan <= '1';
						inf <= '0';
					else				 -- operando2=Infinito
						nan <= '0';
						inf <= '1';
						
					end if;
				else					 -- operando2=Num ou 0
					nan <= '0';
					inf <= '0';
					if ((or_mantissa2='0') and (or_exp2='0')) then	 --operando2==0
						sinaltmp<= sinal1 and sinal2;
					else				 -- operando2==Num
						sinaltmp<= sinal2;
					end if;
				end if;
			else						 -- operando1=Num
				if (and_exp2='1') then			 -- operando2=NaN ou Infinito
					sinaltmp<= sinal2;
					if (or_mantissa2 = '1') then	 -- operando2=NaN
						nan <= '1';
						inf <= '0';
					else				 -- operando2=Infinito
						nan <= '0';
						inf <= '1';
						
					end if;
					
				else					 -- operando2=Num ou 0
					sinaltmp<= sinal_mux;		
					nan <= '0';
					inf <= '0';
			 	end if;
			end if;
		end if;

	end process;

	EX1_sinal <= sinaltmp;
	EX1_Inf <= inf;
	EX1_NaN <= nan;
end mda;
