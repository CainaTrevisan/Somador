library ieee;
	use ieee.std_logic_1164.all;
	use ieee.numeric_std.all;

	entity somador is
		port (	clk, rst: in std_logic;
				float1, float2: in std_logic_vector(31 downto 0);
				result: out std_logic_vector(31 downto 0));
	end somador;

	architecture somador_arch of somador is
			
		signal sinal1, sinal2: std_logic;
		signal expoente1, expoente2 : std_logic_vector (8 downto 0);
		
		signal mantissa1_init, mantissa2_init: std_logic_vector (22 downto 0);
		signal mantissa1, mantissa2: std_logic_vector (23 downto 0);
		
		signal rl, sl, vaium2, not_vaium2, out_and1, out_and2, not_out_and2, not_sel_sub, sel_sub :std_logic;
		signal will_round, or_dif_exp: std_logic;
		signal will_round_extd: std_logic_vector(24 downto 0);
		signal and_exp1, and_exp2, or_mantissa1, or_mantissa2: std_logic;

		signal out_compl3: std_logic_vector(8 downto 0);
	
		signal count: std_logic_vector(7 downto 0);
		signal vaium2_extd, out_and2_extd, out_fix_exp: std_logic_vector(7 downto 0);


		signal out_mux1, out_compl1: std_logic_vector(23 downto 0);
	   	signal out_sum2: std_logic_vector(24 downto 0);
		signal out_compl2: std_logic_vector(24 downto 0);
		signal out_shift2: std_logic_vector(24 downto 0);
	   	signal out_shift3, out_shift4: std_logic_vector(24 downto 0);

		-- Signals stage 1
		signal EX1_g, EX1_r, EX1_s, EX1_out_comp, EX1_out_mux4, EX1_and_sel4: std_logic;
		signal EX1_out_mux2:std_logic_vector(23 downto 0);
		signal EX1_out_compl1, EX1_out_shift1 : std_logic_vector(24 downto 0);
		signal EX1_out_mux3: std_logic_vector(8 downto 0);
		signal EX1_out_sub: std_logic_vector(8 downto 0);
		signal EX1_NaN, EX1_Inf, EX1_sinal: std_logic;
 
		-- Signals stage 2
		signal EX2_out_comp, EX2_vaium1, EX2_g, EX2_r, EX2_s, EX2_out_mux4: std_logic;
		signal EX2_out_mux3: std_logic_vector(8 downto 0);
		signal EX2_out_mux2: std_logic_vector(23 downto 0);
		signal EX2_out_mux2_ext: std_logic_vector(24 downto 0);
		signal EX2_out_compl1, EX2_out_sum1, EX2_out_shift1 : std_logic_vector(24 downto 0);
		signal EX2_out_sub: std_logic_vector(8 downto 0);
		signal EX2_NaN, EX2_Inf1, EX2_Inf2, EX2_sinal: std_logic;

		-- Signals stage 3
		signal EX3_out_comp, EX3_vaium1, EX3_g, EX3_r, EX3_s, EX3_out_mux4: std_logic;
		signal EX3_out_mux3: std_logic_vector(8 downto 0);
		signal EX3_out_mux2 : std_logic_vector(23 downto 0);
		signal EX3_out_shift1, EX3_out_sum1, EX3_out_compl1 : std_logic_vector(24 downto 0);
		signal EX3_out_sub: std_logic_vector(8 downto 0);
		signal EX3_NaN, EX3_Inf1, EX3_Inf2, EX3_sinal: std_logic;

	begin

		-----------------------------------------------------------------------------
		-- First Stage: EX1
		-----------------------------------------------------------------------------

		-- Unpack float1
		sinal1 <= float1(31);
		expoente1 <= '0' & float1(30 downto 23);
		mantissa1_init <= float1(22 downto 0);
		mantissa1 <= (float1(30) or float1(29) or float1(28) or float1(27) or float1(26) or float1(25) or float1(24) or float1(23)) & mantissa1_init;

		-- Unpack float2
		sinal2 <= float2(31);
		expoente2 <= '0' & float2(30 downto 23);
		mantissa2_init <= float2(22 downto 0);
		mantissa2 <= (float2(30) or float2(29) or float2(28) or float2(27) or float2(26) or float2(25) or float2(24) or float2(23)) & mantissa2_init;

		

		-- Subtract exponents 
		
		Sub: entity work.sub_exp port map(expoente1, expoente2, EX1_out_sub);

		-- Comp: entity work.comp port map(sinal1, sinal2, out_comp);
		EX1_out_comp <= sinal1 xor sinal2;

		sel_sub <= EX1_out_sub(8);

		EX1_and_sel4 <= EX1_out_comp and sel_sub;	

		Mux4: entity work.mux1  port map(sinal1, sinal2, EX1_and_sel4, EX1_out_mux4);
		marido_de_aluguel: entity work.marido_de_aluguel port map (clk, sinal1, sinal2, float1(30 downto 23),
		float2(30 downto 23), mantissa1_init, mantissa2_init, EX1_out_mux4, EX1_Inf, EX1_NaN, EX1_sinal);

		Mux1: entity work.mux24 port map(mantissa1, mantissa2, sel_sub, out_mux1);

		Compl2: entity work.compl2_8 port map(EX1_out_sub, sel_sub, out_compl3); -- n shift da mantissa

		Compl1: entity work.compl2_24 port map(out_mux1, EX1_out_comp, EX1_out_compl1);
		Shift_Maitissa: entity work.shift_mantissa port map(EX1_out_compl1, out_compl3, EX1_g, EX1_r, EX1_s, EX1_out_shift1);

--		not_sel_sub <= not sel_sub;

		Mux2: entity work.mux24 port map(mantissa2, mantissa1, sel_sub, EX1_out_mux2);
		Mux3: entity work.mux8 port map(expoente1, expoente2, sel_sub, EX1_out_mux3);

		--EX1_Inf1 <= and_exp1 and (not or_mantissa1);
		--EX1_Inf2 <= and_exp2 and (not or_mantissa2);
		--EX1_NaN <= (and_exp1 and or_mantissa1) or (and_exp2 and or_mantissa2) or (EX1_Inf1 and EX2_Inf2 and EX1_out_comp);

		
		EX1: process(clk)
		begin
			if rising_edge(clk) then
				if rst = '1' then
					
					EX2_g <= '0';
					EX2_s <= '0';
					EX2_r <= '0';
					EX2_out_comp <= '0';
					EX2_out_compl1 <= (others => '0');
					EX2_out_shift1 <= (others => '0');
					EX2_out_mux2 <= (others => '0');
					EX2_out_mux3 <= (others => '0');
					EX2_sinal <= '0';
					EX2_out_sub <= (others => '0');

				else

					EX2_g <= EX1_g;
					EX2_s <= EX1_s;
					EX2_r <= EX1_r;
					EX2_out_comp <= EX1_out_comp;
					EX2_out_compl1 <= EX1_out_compl1;
					EX2_out_shift1 <= EX1_out_shift1;
					EX2_out_mux2 <= EX1_out_mux2;
					EX2_out_mux3 <= EX1_out_mux3;
					EX2_sinal <= EX1_sinal;
					EX2_out_sub <= EX1_out_sub;
	
				end if;
			end if;
		end process;

		-----------------------------------------------------------------------------
		-- Second Stage: EX2
		-----------------------------------------------------------------------------
--
		EX2_out_mux2_ext <= '0' & EX2_out_mux2;
		Sum1: entity work.sum port map(EX2_out_shift1, EX2_out_mux2_ext, EX2_out_sum1, EX2_vaium1);

		EX2: process(clk)
		begin
			if rising_edge(clk) then
				if rst = '1' then
					
					EX3_out_comp <= '0';
					EX3_vaium1 <= '0';
					EX3_g <= '0';
					EX3_s <= '0';
					EX3_r <= '0';
					EX3_out_sum1 <= (others => '0');
					EX3_out_compl1 <= (others => '0');
					EX3_out_mux2 <= (others => '0');
					EX3_sinal <= '0';
					EX3_out_sub <= (others => '0');

				else

					EX3_out_comp <= EX2_out_comp;
					EX3_vaium1 <= EX2_vaium1;
					EX3_g <= EX2_g;
					EX3_s <= EX2_s;
					EX3_r <= EX2_r;
					EX3_out_sum1 <= EX2_out_sum1;
					EX3_out_compl1 <= EX2_out_compl1;
					EX3_out_mux2 <= EX2_out_mux2;
					EX3_out_mux3 <= EX2_out_mux3;
					EX3_sinal <= EX2_sinal;
					EX3_out_sub <= EX2_out_sub;

				end if;
			end if;
		end process;
--
--		-----------------------------------------------------------------------------
--		-- Third Stage: EX3
--		-----------------------------------------------------------------------------
--		
	
		or_dif_exp <= EX3_out_sub(0) or 
					  EX3_out_sub(1) or 
					  EX3_out_sub(2) or 
					  EX3_out_sub(3) or 
					  EX3_out_sub(4) or 
					  EX3_out_sub(5) or 
					  EX3_out_sub(6) or 
					  EX3_out_sub(7) or 
					  EX3_out_sub(8);

		out_and1 <= (not EX3_vaium1) and EX3_out_sum1(24) and EX3_out_comp and (not or_dif_exp);
		out_and2 <= EX3_vaium1 and (not EX3_out_comp);

		Compl3: entity work.compl2_25 port map(EX3_out_sum1, out_and1, out_compl2);

		not_out_and2 <= not out_and2;
		not_vaium2 <= not vaium2;

		Shift_Sum_Right: entity work.shift_one_right port map(out_compl2, out_and2, out_shift2);
		Shift_Sum_Left: entity work.shift_sum_left port map(out_shift2, not_out_and2, EX3_g, out_shift3, count);
		
		Pandora: entity work.pandora port map(out_shift3(0), EX3_r, EX3_s, EX3_g, out_and2, count, rl, sl);		


		Round: entity work.round port map(out_shift3(0), rl, sl, EX3_out_mux4, will_round);
		will_round_extd <= "000000000000000000000000" & will_round;
		Sum2: entity work.sum port map(will_round_extd, out_shift3, out_sum2, vaium2);
		Shift_One_Right: entity work.shift_one_right port map(out_sum2, vaium2, out_shift4);

		vaium2_extd <= "0000000" & vaium2;
		out_and2_extd <= "0000000" & out_and2;

		Fix_Exp: entity  work.fix_exp port map(EX3_out_mux3, out_and2_extd, vaium2_extd, count, out_fix_exp);


		result <= EX3_sinal & out_fix_exp & out_shift4(22 downto 0); 

	end somador_arch;
