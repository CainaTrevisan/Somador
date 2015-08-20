library IEEE;
use IEEE.std_logic_1164.all;

entity somador_tb is
end somador_tb;

architecture arch_somador of somador_tb is

	component somador is
		port( 
				clk, rst : in std_logic;
				float1, float2: in std_logic_vector(31 downto 0);
				result: out std_logic_vector(31 downto 0));
	end component somador;

	signal clock: std_logic := '0';
	signal reset: std_logic := '1'; 
	signal f1, f2 ,res : std_logic_vector(31 downto 0) := (others => '0');

begin
	clock <= not clock after 200 NS;

	reset <= '1', '0' after 250 NS;

	f1 <= "00111111110000000000000000000000" after 500 NS;
	f2 <= "01000000001000000000000000000000" after 500 NS;

	A: somador port map (clock, reset, f1, f2, res );

end arch_somador;
