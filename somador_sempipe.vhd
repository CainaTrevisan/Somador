library ieee;
	use ieee.std_logic_1164.all;
	use ieee.numeric_std.all;

	entity somador is
		port (	clk, rst: in std_logic;
				float1, float2: in std_logic_vector(31 downto 0);
				result: out std_logic_vector(31 downto 0));
	end somador;

	architecture somador_arch of somador is
			
		signal sinal1, sinal2, out_mux4: std_logic;
		signal expoente1, expoente2 : std_logic_vector (7 downto 0);
		
		-- Verificar tamanho dos campos
		signal mantissa1, mantissa2: std_logic_vector (22 downto 0);
		
		signal out_comp, g, r, s, rl, sl, vaium1, vaium2, not_vaium2, out_and1, out_and2, not_out_and2, not_sel_sub, sel_sub :std_logic;	
		signal saida_calc: std_logic_vector(22 downto 0);

		signal out_sub, out_mux3: std_logic_vector(7 downto 0);
	
		signal count: std_logic_vector(1 downto 0);

		signal out_mux1, out_mux2, out_compl1: std_logic_vector(22 downto 0);
	   	signal out_shitf1, out_sum2: std_logic_vector(22 downto 0);
		signal out_sum1, out_compl2, out_shift2: std_logic_vector(22 downto 0);
	   	signal out_shift3, out_shift4: std_logic_vector(22 downto 0);

	begin

		-- Unpack float1
		sinal1 <= float1(31);
		expoente1 <= float1(30 downto 23);
		mantissa1 <= float1(22 downto 0);

		-- Unpack float2
		sinal2 <= float2(31);
		expoente2 <= float2(30 downto 23);
		mantissa2 <= float2(22 downto 0);

		not_sel_sub <= not sel_sub;

		Mux1: entity work.mux23 port map(mantissa1, mantissa2, sel_sub, out_mux1);
		Mux2: entity work.mux23 port map(mantissa1, mantissa1, not_sel_sub, out_mux2);
		Mux3: entity work.mux8 port map(expoente1, expoente2, not_sel_sub, out_mux3);
		Mux4: entity work.mux1  port map(sinal1, sinal2, out_comp, out_mux4);
		
		Sub: entity work.sub_exp port map(expoente1, expoente2, out_sub);

		sel_sub <= out_sub(7);
		
		-- Comp: entity work.comp port map(sinal1, sinal2, out_comp);
		out_comp <= sinal1 xor sinal2;
		out_and1 <= vaium1 and (out_sum1(22) and (not out_comp));
		out_and2 <= vaium1 and out_comp;

		Compl1: entity work.compl2 port map(out_mux1, out_comp, out_compl1);
		Compl2: entity work.compl2 port map(out_sum1, out_and1, out_compl2);

		not_out_and2 <= not out_and2;
		not_vaium2 <= not vaium2;

		Shift_mantissa: entity work.shift_mantissa port map(out_compl1, out_sub, out_shift1, g, r, s);
		Shift2: entity work.shift2 port map(out_compl2, out_and2, out_shift2);
		Shift3: entity work.shift3 port map(out_shift2, not_out_and2, out_shift3, count);
		Shift4: entity work.shift4 port map(out_sum2, not_vaium2, out_shift4);

		Round: entity work.round port map(out_compl2(0), rl, sl, out_mux4, saida_calc);
	
		Pandora: entity work.pandora port map(out_compl2(0), r, s, g, out_and2, rl, sl);		
		Sum1: entity work.sum port map(out_shitf1, out_mux2, out_sum1, vaium1);
		Sum2: entity work.sum port map(saida_calc, out_mux2, out_sum1, vaium1);

		result <= sinal3 & out_mux3 & out_shift4; 

	end somador_arch;

	architecture arch_somador of somador is
	begin
		clk <= not clk after 200 NS;
		rst <= '1', '0' after 250 NS;
		A: somador port map(clk, rst, result);
	end arch_somador;
