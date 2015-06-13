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
		if sel = '0' then
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
entity sum is
	port (a, b: in std_logic_vector(22 downto 0);
			 r: out std_logic_vector(22 downto 0);
			ow: out std_logic);
end sum;

architecture func of sum is
	signal result: std_logic_vector(23 downto 0);
begin
	process(a,b)	
	begin
		result <= std_logic_vector(unsigned('0'&a)+unsigned('0'&b));
		r <= result (22 downto 0);
		ow <= result(23);
	end process;
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
	signal count: std_logic_vector(7 downto 0);
end shift_mantissa;

architecture sh_mant of shift_mantissa is
begin
	process (a, n)
	begin	
		count <= n;
		tmp <= a;
		while (unsigned(count)>0) loop
			tmp <= '0' & tmp( 22 downto 1);
			count <= std_logic_vector(unsigned(count)-1);
		end loop;
		result <= tmp;
	end process;
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
end shift_sum_left;

architecture ssl of shift_sum_left is
begin
	process (a)
	begin
		tmp <= "00000000";
		rtmp <= a;
		if (rtmp(22)/='1') then
			rtmp<= rtmp(21 downto 0) & g;
			tmp <= std_logic_vector(unsigned(tmp)+1);
		end if;
		while (rtmp(22)/='1') loop
			rtmp<= rtmp(21 downto 0) & '0';
			tmp <= std_logic_vector(unsigned(tmp)+1);
		end loop;
		count <= tmp;
		result <= rtmp;
	end process;
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
