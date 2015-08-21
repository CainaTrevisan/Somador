library IEEE;
use IEEE.std_logic_1164.all;

entity somador_tb is
end somador_tb;

architecture arch_somador of somador_tb is

	signal clock: std_logic; --:= '0';
	signal reset: std_logic; --:= '1'; 
	signal f1, f2, f3, res : std_logic_vector(31 downto 0) := (others => '0');

	signal ok, aux: std_logic;
	signal auxv1, auxv2 : std_logic_vector(31 downto 0);
	
	type entrada is record
			a : std_logic_vector(31 downto 0);
			b : std_logic_vector(31 downto 0);
			c : std_logic_vector(31 downto 0);
	end record;

	type test_array is array(positive range <>) of entrada;

	constant test_vector : test_array := (

-- Inf
		(x"7f800000", x"7f800000", x"7f800000"), -- inf + inf = inf
		(x"7f800000", x"ff800000", x"7fffffff"), -- inf + -inf = NaN	
		(x"ff800000", x"7f800000", x"7fffffff"), -- -inf + inf = NaN
		(x"ff800000", x"ff800000", x"ff800000"), -- -inf + -inf = -inf
-- inf + NaN		
		(x"7f800000", x"7fffffff", x"7fffffff"), -- inf + NaN = NaN
		(x"7f800000", x"ffffffff", x"ffffffff"), -- inf + -NaN = -NaN
		(x"ff800000", x"7fffffff", x"7fffffff"), -- -inf + NaN = NaN
		(x"ff800000", x"ffffffff", x"ffffffff"), -- -inf + -NaN = -NaN
-- NaN + inf
		(x"7fffffff", x"7f800000", x"7fffffff"), -- NaN + inf = NaN
		(x"ffffffff", x"7f800000", x"ffffffff"), -- -NaN + inf = -NaN
		(x"7fffffff", x"ff800000", x"7fffffff"), -- NaN + -inf = NaN
		(x"ffffffff", x"ff800000", x"ffffffff"), -- -NaN + -inf = -NaN		

-- TODO: Descobrir o que fazer com os NaN
-- NaN
		(x"7fffffff", x"7fffffff", x"7fffffff"), -- NaN + NaN = NaN
		(x"7fffffff", x"ffffffff", x"7fffffff"), -- NaN + -NaN = NaN
		(x"ffffffff", x"7fffffff", x"ffffffff"), -- -NaN + NaN = -NaN
		(x"ffffffff", x"ffffffff", x"ffffffff"), -- -NaN + -NaN = -NaN		
-- Inf + 0
		(x"00000000", x"00000000", x"00000000"),
		(x"7f800000", x"00000000", x"7f800000"), -- inf + 0 = inf
		(x"ff800000", x"00000000", x"ff800000"), -- -inf + 0 = -inf	
		(x"00000000", x"7f800000", x"7f800000"), -- 0 + inf = inf
		(x"00000000", x"ff800000", x"ff800000"), -- 0 + -inf = -inf
-- inf + -0	
		(x"00000000", x"00000000", x"00000000"),
		(x"7f800000", x"80000000", x"7f800000"), -- inf + -0 = inf
		(x"ff800000", x"80000000", x"ff800000"), -- -inf + -0 = -inf	
		(x"80000000", x"7f800000", x"7f800000"), -- -0 + inf = inf
		(x"80000000", x"ff800000", x"ff800000"), -- -0 + -inf = -inf
-- NaN + 0
		(x"00000000", x"00000000", x"00000000"),
		(x"7f800000", x"00000000", x"7f800000"), -- NaN + 0 = NaN
		(x"ff800000", x"00000000", x"ff800000"), -- -NaN + 0 = -NaN	
		(x"00000000", x"7f800000", x"7f800000"), -- 0 + NaN = NaN
		(x"00000000", x"ff800000", x"ff800000"), -- 0 + -NaN = -NaN
-- NaN + -0	
		(x"00000000", x"00000000", x"00000000"),
		(x"7f800000", x"80000000", x"7f800000"), -- NaN + -0 = NaN
		(x"ff800000", x"80000000", x"ff800000"), -- -NaN + -0 = -NaN	
		(x"80000000", x"7f800000", x"7f800000"), -- -0 + NaN = NaN
		(x"80000000", x"ff800000", x"ff800000"), -- -0 + -NaN = -NaN
-- inf + 1		
		(x"00000000", x"00000000", x"00000000"),
		(x"7f800000", x"3f800000", x"7f800000"), -- inf + 1 = inf
		(x"ff800000", x"3f800000", x"ff800000"), -- -inf + 1 = -inf	
		(x"3f800000", x"7f800000", x"7f800000"), -- 1 + inf = inf
		(x"3f800000", x"ff800000", x"ff800000"), -- 1 + -inf = -inf
-- inf + -1		
		(x"00000000", x"00000000", x"00000000"),
		(x"7f800000", x"bf800000", x"7f800000"), -- inf + -1 = inf
		(x"ff800000", x"bf800000", x"ff800000"), -- -inf + -1 = -inf	
		(x"bf800000", x"7f800000", x"7f800000"), -- -1 + inf = inf
		(x"bf800000", x"ff800000", x"ff800000"), -- -1 + -inf = -inf
-- NaN + 1
		(x"00000000", x"00000000", x"00000000"),
		(x"7f800000", x"3f800000", x"7f800000"), -- NaN + 1 = NaN
		(x"ff800000", x"3f800000", x"ff800000"), -- -NaN + 1 = -NaN	
		(x"3f800000", x"7f800000", x"7f800000"), -- 1 + NaN = NaN
		(x"3f800000", x"ff800000", x"ff800000"), -- 1 + -NaN = -NaN
-- Nan + -1		
		(x"00000000", x"00000000", x"00000000"),
		(x"7f800000", x"bf800000", x"7f800000"), -- NaN + -1 = NaN
		(x"ff800000", x"bf800000", x"ff800000"), -- -NaN + -1 = -NaN	
		(x"bf800000", x"7f800000", x"7f800000"), -- -1 + NaN = NaN
		(x"bf800000", x"ff800000", x"ff800000"), -- -1 + -NaN = -NaN
		(x"00000000", x"00000000", x"00000000"),
-- inf + 7bffffff		
		(x"7f800000", x"7bffffff", x"7f800000"), -- inf + 7bffffff = inf
		(x"ff800000", x"7bffffff", x"ff800000"), -- -inf + 7bffffff = -inf	
		(x"7bffffff", x"7f800000", x"7f800000"), -- 7bffffff + inf = inf
		(x"7bffffff", x"ff800000", x"ff800000"), -- 7bffffff + -inf = -inf
-- inf + fbffffff		
		(x"00000000", x"00000000", x"00000000"),
		(x"7f800000", x"fbffffff", x"7f800000"), -- inf + -fbffffff = inf
		(x"ff800000", x"fbffffff", x"ff800000"), -- -inf + -fbffffff = -inf	
		(x"fbffffff", x"7f800000", x"7f800000"), -- -fbffffff + inf = inf
		(x"fbffffff", x"ff800000", x"ff800000"), -- -fbffffff + -inf = -inf
-- NaN + 7bffffff
		(x"00000000", x"00000000", x"00000000"),
		(x"7f800000", x"7bffffff", x"7f800000"), -- NaN + 7bffffff = NaN
		(x"ff800000", x"7bffffff", x"ff800000"), -- -NaN + 7bffffff = -NaN	
		(x"7bffffff", x"7f800000", x"7f800000"), -- 7bffffff + NaN = NaN
		(x"7bffffff", x"ff800000", x"ff800000"), -- 7bffffff + -NaN = -NaN
-- Nan + fbffffff		
		(x"00000000", x"00000000", x"00000000"),
		(x"7f800000", x"fbffffff", x"7f800000"), -- NaN + fbffffff = NaN
		(x"ff800000", x"fbffffff", x"ff800000"), -- -NaN + fbffffff = -NaN	
		(x"fbffffff", x"7f800000", x"7f800000"), -- fbffffff + NaN = NaN
		(x"fbffffff", x"ff800000", x"ff800000"), -- fbffffff + -NaN = -NaN
		(x"00000000", x"00000000", x"00000000"),
-- inf + 007fffff		
		(x"7f800000", x"007fffff", x"7f800000"), -- inf + 007fffff = inf
		(x"ff800000", x"007fffff", x"ff800000"), -- -inf + 007fffff = -inf	
		(x"007fffff", x"7f800000", x"7f800000"), -- 007fffff + inf = inf
		(x"007fffff", x"ff800000", x"ff800000"), -- 007fffff + -inf = -inf
-- inf + 807fffff		
		(x"00000000", x"00000000", x"00000000"),
		(x"7f800000", x"807fffff", x"7f800000"), -- inf + 807fffff = inf
		(x"ff800000", x"807fffff", x"ff800000"), -- -inf + 807fffff = -inf	
		(x"807fffff", x"7f800000", x"7f800000"), -- 807fffff + inf = inf
		(x"807fffff", x"ff800000", x"ff800000"), -- 807fffff + -inf = -inf
-- NaN + 007fffff
		(x"00000000", x"00000000", x"00000000"),
		(x"7f800000", x"007fffff", x"7f800000"), -- NaN + 007fffff = NaN
		(x"ff800000", x"007fffff", x"ff800000"), -- -NaN + 007fffff = -NaN	
		(x"007fffff", x"7f800000", x"7f800000"), -- 007fffff + NaN = NaN
		(x"007fffff", x"ff800000", x"ff800000"), -- 007fffff + -NaN = -NaN
-- Nan + 807fffff		
		(x"00000000", x"00000000", x"00000000"),
		(x"7f800000", x"807fffff", x"7f800000"), -- NaN + 807fffff = NaN
		(x"ff800000", x"807fffff", x"ff800000"), -- -NaN + 807fffff = -NaN	
		(x"807fffff", x"7f800000", x"7f800000"), -- 807fffff + NaN = NaN
		(x"807fffff", x"ff800000", x"ff800000"), -- 807fffff + -NaN = -NaN
		(x"00000000", x"00000000", x"00000000"),
-- 0 + -0		
		(x"00000000", x"00000000", x"00000000"), -- 0 + 0 = 0
		(x"00000000", x"80000000", x"00000000"), -- 0 + -0 = 0
		(x"80000000", x"00000000", x"00000000"), -- -0 + 0 = 0
		(x"80000000", x"80000000", x"80000000"), -- -0 + -0 = -0
		(x"00000000", x"00000000", x"00000000"),

-- Overflow
		(x"7f000000", x"7f000000", x"7f800000"), -- 7f000000 + 7f000000 = inf
		(x"7f000000", x"ff000000", x"00000000"), -- 7f000000 + ff000000 = 0
		(x"ff000000", x"7f000000", x"00000000"), -- ff000000 + 7f000000 = 0
		(x"ff000000", x"ff000000", x"ff800000"), -- ff000000 + ff000000 = -inf
		(x"00000000", x"00000000", x"00000000"),
		
		(x"7e800000", x"7f000000", x"7f800000"), -- 7e800000 + 7f000000 = inf
		(x"7e800000", x"fe800000", x"00000000"), -- 7e800000 + fe800000 = 0
		(x"fe800000", x"7e800000", x"00000000"), -- fe800000 + 7e800000 = 0
		(x"ff000000", x"fe800000", x"ff800000"), -- ff000000 + fe800000 = -inf
		(x"00000000", x"00000000", x"00000000"),

-- Denormalized
		(x"00000001", x"00000001", x"00000010"), -- 00000001 + 00000001 = 00000010
		(x"00400000", x"00400000", x"00400000"), -- 00400000 + 00400000 = 
--Overflow + Round
		(x"7bffffff", x"7bffffff", x"7f800000"), -- 7bffffff + 7bffffff = inf
		(x"fbffffff", x"7bffffff", x"00000000"), -- fbffffff + 7bffffff = 0
		(x"7bffffff", x"fbffffff", x"00000000"), -- 7bffffff + fbffffff = 0
		(x"fbffffff", x"fbffffff", x"ff800000"), -- fbffffff + fbffffff = -inf
		
		(x"7f7fffff", x"7f7fffff", x"7f800000"), -- 7f7fffff + 7f7fffff = inf
		(x"7f7fffff", x"ff7fffff", x"00000000"), -- 7f7fffff + ff7fffff = 0
		(x"ff7fffff", x"7f7fffff", x"00000000"), -- ff7fffff + 7f7fffff = 0
		(x"ff7fffff", x"ff7fffff", x"ff800000"), -- ff7fffff + ff7fffff = -inf
		
		(x"bfe00000", x"40000000", x"3e800000"),
		(x"3fe00003", x"c0000007", x"be80002c"),
		(x"3fe00003", x"c0000005", x"be80001b"),
		(x"86844a7f", x"66844a7f", x"66844a7f"), -- 4.9762344E-35 	3.123633E23
		(x"4004802b", x"3f04803d", x"4025a03a"), -- 2.0703228 		0.51758176 --		2.5879045
		(x"55a5a03a", x"550da032", x"55ec7053"), -- 9.7324483E12 	2.27634483E13 	3.24958966E13
		(x"7f7fffff", x"7f7fffff", x"00000000"),
		(x"80000001", x"80000001", x"00000000"),
		(x"ff7fffff", x"ff7fffff", x"00000000"),
		(x"00000000", x"00000000", x"00000000")
	);

begin
--	clock <= not clock after 200 NS;

	reset <= '1', '0' after 250 NS;

	clk: process
	begin
		clock <= '0';
		wait for 200 ns;
		clock <= '1';
		wait for 200 ns;
	end process clk;	

--	rst: process
--	begin
--		reset <= '0';
--		wait for 250 ns;
--		reset <= '1';
--	end process rst;	
	
--	f2 <= "00111111111000000000000000000000" after 500 NS;
--	f1 <= "11000000000000000000000000000000" after 500 NS;

	Gera_entradas: process
		variable v : entrada;
	begin

		f1 <= (others => '0');
		f2 <= (others => '0');
		f3 <= (others => '0');

		for i in test_vector'range loop
			wait until rising_edge(clock);
			v := test_vector(i);
			f1 <= v.a;
			f2 <= v.b;
			f3 <= v.c;

		end loop;

--	f2 <= "10111111111000000000000000000000";
--	f1 <= "01000000000000000000000000000000";
--	f2 <= "00111111111000000000000000000011" after 950 NS;
--	f1 <= "11000000000000000000000000000111" after 950 NS;
--	f2 <= "00111111111000000000000000000011" after 1150 NS;
--	f1 <= "11000000000000000000000000000101" after 1150 NS;
	end process Gera_entradas;
	
	segura1: process
	begin
		wait until rising_edge(clock);
		auxv1 <= f3;
	end process segura1;
	
	segura2: process
	begin
		wait until rising_edge(clock);
		auxv2 <= auxv1;
	end process segura2;
	
	compara_resultados: process(auxv2, res)
	begin
		if (auxv2 = res) then
			aux <= '1';
		else
			aux <= '0';
		end if;
	end process compara_resultados;
	ok <= aux;
	
	A: entity work.somador port map (clock, reset, f1, f2, res );
end arch_somador;
