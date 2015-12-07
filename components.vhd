-------------------------------------------------------------------------------
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

-------------------------------------------------------------------------------
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
--TODO: para padronizacao melhor mudar sel='0' para sel='1'
		if sel = '0' then
			c <= a;
		else
			c <= b;
		end if;
	end process;
	
end behav_mux8;

-------------------------------------------------------------------------------
library IEEE;
use IEEE.std_logic_1164.all;
use IEEE.numeric_std.all;

entity sub_exp is
	port(a, b:	in  std_logic_vector(8 downto 0);
	     result: 	out std_logic_vector (8 DOWNTO 0));
end sub_exp;

architecture functional of sub_exp is
begin
	-- Soma a com o complemento de 2 de b
    result <= std_logic_vector(unsigned(a)+unsigned(unsigned(not b)+1));
end architecture functional;

-------------------------------------------------------------------------------
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

-------------------------------------------------------------------------------
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

-------------------------------------------------------------------------------
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

-------------------------------------------------------------------------------
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
	ow <= result(24);
end func;

-------------------------------------------------------------------------------
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

end sh_mant;

-------------------------------------------------------------------------------
library IEEE;
use IEEE.std_logic_1164.all;
use IEEE.numeric_std.all;

entity shift_sum_left is
	port(enable: in std_logic;
	     exp_preliminar: in std_logic_vector(8 downto 0);
		 both_denormal : in std_logic;
	     denormal : in std_logic;
	     mantissa_zerada :in std_logic;
	     a	: in std_logic_vector(24 downto 0);
	     en : in std_logic;
	     g	: in std_logic;
	     result	: out std_logic_vector(24 downto 0);
	     count	: out std_logic_vector(7 downto 0);
	     flag_denormal : out std_logic);
end shift_sum_left;

architecture ssl of shift_sum_left is
	signal count_tmp: std_logic_vector(7 downto 0);
	signal tmp: std_logic_vector(7 downto 0);
	signal enable_tmp: std_logic_vector(1 downto 0);
	signal rtmp: std_logic_vector(24 downto 0);
	signal m23, m22, m21, m20, m19, m18, m17, m16, m15, m14, m13, m12, m11,
   		   m10, m9, m8, m7, m6, m5, m4, m3, m2, m1, m0: std_logic;
	signal shamt, shamt_tmp: std_logic_vector(4 downto 0);
	signal max_shamt: std_logic_vector (8 downto 0);
begin
	enable_tmp <= '0' & enable;

	process (exp_preliminar)
	begin
		if (exp_preliminar = x"00") then
			max_shamt <= exp_preliminar;
		else
			max_shamt <= std_logic_vector(unsigned(exp_preliminar) - 1);
		end if;
	end process;

	process(a, max_shamt)
	variable n : std_logic_vector(4 downto 0);
	variable f: std_logic;
	begin
		if ((a(23) = '1') or  ( max_shamt="000000000")) then
			n := "00000"; f := not(a(23));
	   	elsif ((a(22) = '1') or  ( max_shamt="000000001")) then
			 n := "00001"; f := not(a(22));	
	   	elsif ((a(21) = '1') or  ( max_shamt="000000010")) then
			 n := "00010"; f := not(a(21));	
	   	elsif ((a(20) = '1') or  ( max_shamt="000000011")) then
			 n := "00011"; f := not(a(20));	
	   	elsif ((a(19) = '1') or  ( max_shamt="000000100")) then
			 n := "00100"; f := not(a(19));	
	   	elsif ((a(18) = '1') or  ( max_shamt="000000101")) then
			 n := "00101"; f := not(a(18));	
	   	elsif ((a(17) = '1') or  ( max_shamt="000000110")) then
			 n := "00110"; f := not(a(17));	
	   	elsif ((a(16) = '1') or  ( max_shamt="000000111")) then
			 n := "00111"; f := not(a(16));	
	   	elsif ((a(15) = '1') or  ( max_shamt="000001000")) then
			 n := "01000"; f := not(a(15));	
	   	elsif ((a(14) = '1') or  ( max_shamt="000001001")) then
			 n := "01001"; f := not(a(14));	
	   	elsif ((a(13) = '1') or  ( max_shamt="000001010")) then
			 n := "01010"; f := not(a(13));	
	   	elsif ((a(12) = '1') or  ( max_shamt="000001011")) then
			 n := "01011"; f := not(a(12));	
	   	elsif ((a(11) = '1') or  ( max_shamt="000001100")) then
			 n := "01100"; f := not(a(11));	
	   	elsif ((a(10) = '1') or  ( max_shamt="000001101")) then
			 n := "01101"; f := not(a(10));	
	   	elsif ((a(9) = '1') or  ( max_shamt="000001110")) then
			 n :=  "01110"; f := not(a(9));	
	   	elsif ((a(8) = '1') or  ( max_shamt="000001111")) then
			 n :=  "01111"; f := not(a(8));	
	   	elsif ((a(7) = '1') or  ( max_shamt="000010000")) then
			 n :=  "10000"; f := not(a(7));	
	   	elsif ((a(6) = '1') or  ( max_shamt="000010001")) then
			 n :=  "10001"; f := not(a(6));	
	   	elsif ((a(5) = '1') or  ( max_shamt="000010010")) then
			 n :=  "10010"; f := not(a(5));	
	   	elsif ((a(4) = '1') or  ( max_shamt="000010011")) then
			 n :=  "10011"; f := not(a(4));	
	   	elsif ((a(3) = '1') or  ( max_shamt="000010100")) then
			 n :=  "10100"; f := not(a(3));	
	   	elsif ((a(2) = '1') or  ( max_shamt="000010101")) then
			 n :=  "10101"; f := not(a(2));	
	   	elsif ((a(1) = '1') or  ( max_shamt="000010110")) then
			 n :=  "10110"; f := not(a(1));	
	   	elsif ((a(0) = '1') or  ( max_shamt="000010111")) then
			 n :=  "10111"; f := not(a(0));	
		else n := "11000"; f:='0';	
		end if;
		flag_denormal <= f;
		shamt_tmp <= n;

	end process;

	shamt <= shamt_tmp;

	process (a, shamt, en, denormal, both_denormal)
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


		if ((en='1') and (both_denormal='0') and (mantissa_zerada='0')) or(denormal='1') then
			result <= i_16(25 downto 1);
			count_tmp <= "000" & shamt(4 downto 0);
		else
			result <= a(24 downto 0);
			count_tmp <= "00000000";
		end if;

	end process;
	
	count <= count_tmp;

end ssl;

-------------------------------------------------------------------------------
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
signal
	rl_tmp, sl_tmp : std_logic;
begin
	process(lst_sign, r, s, g, out_and, count)
	begin
		if (out_and = '1') then
			rl_tmp <= lst_sign;
			sl_tmp <= g or s or r;
		elsif (unsigned(count) = 0) then
			rl_tmp <= g;
			sl_tmp <= r or s;
		elsif (unsigned(count) = 1) then
			rl_tmp <= r;
			sl_tmp <= s;
		else 
			rl_tmp <= '0';
			sl_tmp <= '0';
		end if;	
	end process;
	rl <= rl_tmp;
	sl <= sl_tmp;
end pand;

-------------------------------------------------------------------------------
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

-------------------------------------------------------------------------------
library IEEE;
use IEEE.std_logic_1164.all;
use IEEE.numeric_std.all;
entity shift_sum_right is
	port (a		: in std_logic_vector(24 downto 0);
	      enable	: in std_logic;
	      result	: out std_logic_vector(24 downto 0));
end shift_sum_right;

architecture ssr of shift_sum_right is
begin
	process (a, enable)
	begin
		if (enable='1') then
			result <= '0' & a(24 downto 1);
		else
			result <= a;
		end if;
	end process;
end ssr;

-------------------------------------------------------------------------------
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
	process (mst_sign, round, sticky)
	begin
		if ((round = '1') and ( (mst_sign = '1') or (sticky = '1')) ) then
			tmp <= '1';
		else
			tmp <= '0';
		end if;
	end process;
	result <= tmp;
end rd;

-------------------------------------------------------------------------------
library IEEE;
use IEEE.std_logic_1164.all;
use IEEE.numeric_std.all;
entity fix_exp is
	port (exp		: in std_logic_vector(8 downto 0);
		  out_and	: in std_logic_vector(7 downto 0);
	      vaium		: in std_logic_vector(7 downto 0);
	      count		: in std_logic_vector(7 downto 0);
	      result	: out std_logic_vector(7 downto 0);
		  both_denormal : in std_logic;
		  menos_zero : in std_logic;
		  um_implicito : in std_logic );

	signal tmp: std_logic_vector(8 downto 0);
	signal tmp2: std_logic_vector(8 downto 0);
	signal tmp3: std_logic_vector(8 downto 0);

end fix_exp;

architecture rd of fix_exp is
signal normaliza : std_logic_vector(1 downto 0);

begin
	normaliza(1) <= '0';
	normaliza(0) <= both_denormal and um_implicito and menos_zero;

	process (exp, out_and, vaium, count, tmp, tmp2, tmp3, normaliza)
	begin
		-- TODO: oveflow!
		tmp <= std_logic_vector(unsigned(out_and) + unsigned(vaium) + unsigned(exp) + unsigned(normaliza));
  		tmp2 <= std_logic_vector(unsigned(tmp) + unsigned(unsigned(not count)+1));
--		tmp2 <= std_logic_vector((unsigned(tmp) - unsigned(count)));
		if (both_denormal = '1') and (um_implicito='1') then
			tmp3 <= "000000001";
		elsif (both_denormal = '1') and (um_implicito='0') then
			tmp3 <= "000000000";
		elsif ( (out_and(0) = '0') and not ( (both_denormal = '1'))) then --and (um_implicito='1'))) then
			tmp3 <= tmp2;
		else
			tmp3 <= tmp;
		end if;
	end process;
	result <= tmp3(7 downto 0);
end rd;

-------------------------------------------------------------------------------
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
	     EX1_sinal,
	     EX1_menos_zero: out std_logic);
end marido_de_aluguel;

architecture mda of marido_de_aluguel is
signal sinaltmp, nan, inf, menos_zero: std_logic;
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
						inf <= not (sinal1 xor sinal2);
						
					end if;
				else					 -- operando2=Num ou 0
					sinaltmp<= sinal1;
					nan <= '0';
					inf <= '1';
			 	end if;
			end if;
			menos_zero <= '0';
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
					menos_zero <= '0';
				else					 -- operando2=Num ou 0
					nan <= '0';
					inf <= '0';
					if ((or_mantissa2='0') and (or_exp2='0')) then	 --operando2==0
						sinaltmp<= sinal1 and sinal2;
						menos_zero <= sinal1 and sinal2;
					else				 -- operando2==Num
						sinaltmp<= sinal2;
						menos_zero <= '0';
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
				menos_zero <= '0';
			end if;
		end if;

	end process;
	EX1_menos_zero <= menos_zero; 
	EX1_sinal <= sinaltmp;
	EX1_Inf <= inf;
	EX1_NaN <= nan;
end mda;

-------------------------------------------------------------------------------
library IEEE;
use IEEE.std_logic_1164.all;
use IEEE.numeric_std.all;
entity trata_excp is
	port (exp_tmp: in std_logic_vector(7 downto 0);
		  mantissa_tmp: in std_logic_vector(22 downto 0);
		  nan		: in std_logic;
	      inf		: in std_logic;
	      sinal 	: in std_logic;
	      vai_dar_zero : in std_logic;
	      or_dif_exp : in std_logic;
	      sinais_diferentes : in std_logic;
	      menos_zero : in std_logic;
		  result	: out std_logic_vector(31 downto 0));
	
end trata_excp;

architecture te of trata_excp is
signal tmp: std_logic_vector(31 downto 0);
begin

	process (sinal, exp_tmp, mantissa_tmp, nan, inf, vai_dar_zero, or_dif_exp, sinais_diferentes)
	begin

		if (nan = '1') then
			tmp <= "01111111111111111111111111111111";

		elsif (inf = '1') then
			tmp <= sinal & "1111111100000000000000000000000";
	
		elsif ((vai_dar_zero='1') and (or_dif_exp='0') and (sinais_diferentes='1')) then
		
			if (menos_zero = '1') then
				tmp <= "10000000000000000000000000000000";
			else
				tmp <= "00000000000000000000000000000000";
			end if;	
		else
			tmp <= sinal & exp_tmp & mantissa_tmp;

		end if;

	end process;

	result <= tmp;

end te;
