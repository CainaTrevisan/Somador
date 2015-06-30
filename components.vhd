library IEEE;
use IEEE.std_logic_1164.all;

entity mux23 is
	port(a, b: in  std_logic_vector(22 downto 0);
		  sel: in std_logic;
			c: out std_logic_vector(22 downto 0));
end mux23;

architecture behav of mux23 is
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
	port(a, b: in std_logic_vector(7 downto 0);
	     sel: in std_logic;
		 c: out std_logic_vector(7 downto 0));

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
	port(a, b:	in  std_logic_vector(7 downto 0);
	     result: 	out std_logic_vector (7 DOWNTO 0));
end sub_exp;

architecture functional of sub_exp is
begin
  result <= std_logic_vector(unsigned(a)+unsigned(unsigned(not b)+1));
end architecture functional;


library IEEE;
use IEEE.std_logic_1164.all;
use IEEE.numeric_std.all;
entity compl2_23 is
	port(a : in std_logic_vector (22 downto 0);
		 enable: in std_logic;
		 b : out std_logic_vector (22 downto 0));
end compl2_23;

architecture arc of compl2_23 is
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
	port(a : in std_logic_vector (7 downto 0);
		 enable: in std_logic;
		 b : out std_logic_vector (7 downto 0));
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
	port (a, b: in std_logic_vector(22 downto 0);
			 r: out std_logic_vector(22 downto 0);
			ow: out std_logic);
end sum;

architecture func of sum is
	signal result, tmp1, tmp2: std_logic_vector(23 downto 0);
begin
	process(a,b)	
	variable tmp11, tmp22, tmp3 : integer;
	begin
		tmp1 <= '0' & a;
		tmp2 <= '0' & b;
		tmp11 := to_integer(unsigned(a));
		tmp22 := to_integer(unsigned(b));
		tmp3 := (tmp11+tmp22);
		result <= std_logic_vector(to_unsigned(tmp3, 24));
	end process;
	r <= result(22 downto 0);
	ow <= result(23);
end func;




library IEEE;
use IEEE.std_logic_1164.all;
use IEEE.numeric_std.all;
entity shift_mantissa is
	port (a		: in std_logic_vector(22 downto 0);
	      n		: in std_logic_vector(7 downto 0);
	      g, r, s	: out std_logic;
	      result	: out std_logic_vector(22 downto 0));

	signal tmp: std_logic_vector(22 downto 0);
	signal resto: std_logic_vector(22 downto 0) := (others => '0');
end shift_mantissa;

architecture sh_mant of shift_mantissa is
begin

--	resto((to_integer(unsigned(n))-1) downto 0) <= a((to_integer(unsigned(n))-1) downto 0);

	resto <= to_stdlogicvector((to_bitvector(a) sll (22-(to_integer(unsigned(n))))));

	g <= resto(22);
	r <= resto(21);

	s <= resto(20) or
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

	result <= to_stdlogicvector(to_bitvector(a) srl to_integer(unsigned(n)));

--	process (tmp, n)
--	begin	
--		count <= n;
--		tmp <= a;
--		while (unsigned(count)>0) loop
--			tmp <= '0' & tmp( 22 downto 1);
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
entity shift_sum_left is
	port(a	: in std_logic_vector(22 downto 0);
	     g	: in std_logic;
	     result	: out std_logic_vector(22 downto 0);
	     count	: out std_logic_vector(7 downto 0));
	signal tmp: std_logic_vector(7 downto 0);
	signal rtmp: std_logic_vector(22 downto 0);
	signal m22, m21, m20, m19, m18, m17, m16, m15, m14, m13, m12, m11,
   		   m10, m9, m8, m7, m6, m5, m4, m3, m2, m1, m0: std_logic;
	signal shamt: std_logic_vector(4 downto 0);
end shift_sum_left;

architecture ssl of shift_sum_left is
begin
	m22 <= a(22);
    m21 <= (not(m22)) and a(21);
	m20 <= (not(m21)) and a(20);
	m19 <= (not(m20)) and a(19);
	m18 <= (not(m19)) and a(18);
	m17 <= (not(m18)) and a(17);
	m16 <= (not(m17)) and a(16);
	m15 <= (not(m16)) and a(15);
	m14 <= (not(m15)) and a(14);
	m13 <= (not(m14)) and a(13);
	m12 <= (not(m13)) and a(12);
	m11 <= (not(m12)) and a(11);
	m10 <= (not(m11)) and a(10);
	m9 <= (not(m10)) and a(9);
	m8 <= (not(m9)) and a(8);
	m7 <= (not(m8)) and a(7);
	m6 <= (not(m7)) and a(6);
	m5 <= (not(m6)) and a(5);
	m4 <= (not(m5)) and a(4);
	m3 <= (not(m4)) and a(3);
	m2 <= (not(m3)) and a(2);
	m1 <= (not(m2)) and a(1);
	m0 <= (not(m1)) and a(0);

	shamt(0) <= m22 or m20 or m18 or m16 or m14 or m12 or m10 or m8 or m6 or m4 or m2 or m0;
	shamt(1) <= m22 or m19 or m18 or m15 or m14 or m11 or m10 or m7 or m6 or m3 or m1;
	shamt(2) <= m22 or m21 or m20 or m15 or m14 or m13 or m12 or m7 or m6 or m5 or m4;
	shamt(3) <= m15 or m14 or m13 or m12 or m11 or m10 or m9 or m8;
	shamt(4) <= m22 or m21 or m20 or m19 or m18 or m17 or m16;
		

	process (a)
	variable i_1, i_2, i_4, i_8, i_16 : std_logic_vector(22 downto 0);
	begin
		if shamt(0) = '1' then i_1  := a(21 downto 0) & b"0";
		else                   i_1  := a;
		end if; 
		if shamt(1) = '1' then i_2  := i_1(20 downto 0) & b"00";
		else                   i_2  := i_1;
		end if; 
		if shamt(2) = '1' then i_4  := i_2(18 downto 0) & b"0000";
		else                   i_4  := i_2;
		end if; 
		if shamt(3) = '1' then i_8  := i_4(14 downto 0) & b"00000000";
		else                   i_8  := i_4;
		end if; 
		if shamt(4) = '1' then i_16 := i_8(6 downto 0) & b"0000000000000000";
		else                   i_16 := i_8;
		end if; 

		result <= i_16;
		count <= "000" & shamt;

	end process;
	

--	process (a)
--	begin
--		tmp <= "00000000";
--		rtmp <= a;
--		if (rtmp(22)/='1') then
--			rtmp<= rtmp(21 downto 0) & g;
--			tmp <= std_logic_vector(unsigned(tmp)+1);
--		end if;
--		while (rtmp(22)/='1') loop
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
	port (a		: in std_logic_vector(22 downto 0);
	      enable	: in std_logic;
	      result	: out std_logic_vector(22 downto 0));
end shift_one_right;

architecture sor of shift_one_right is
begin
	process (a, enable)
	begin
		if (enable='1') then
			result <= '1' & a(22 downto 1);
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
		if ( ((round = '1') and (mst_sign = '1')) or ((round = '1') and (sticky = '1')) ) then
			tmp <= '1';
		else
			tmp <= '0';
		end if;
		result <= tmp;
	end process;
end rd;


library IEEE;
use IEEE.std_logic_1164.all;
use IEEE.numeric_std.all;
entity fix_exp is
	port (exp		: in std_logic_vector(7 downto 0);
		  out_and	: in std_logic_vector(7 downto 0);
	      vaium		: in std_logic_vector(7 downto 0);
	      count		: in std_logic_vector(7 downto 0);
	      result	: out std_logic_vector(7 downto 0));

	signal tmp: std_logic_vector(7 downto 0);

end fix_exp;

architecture rd of fix_exp is
begin
	process (exp, out_and, vaium, count)
	begin
		-- TODO: oveflow!
		tmp <= std_logic_vector(unsigned(out_and) + unsigned(vaium) + unsigned(exp));
		if ( vaium(0) = '1') then
			result <= std_logic_vector(unsigned(tmp) - unsigned(count));
		else
			result <= tmp;
		end if;
	end process;
end rd;
