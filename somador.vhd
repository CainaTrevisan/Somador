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
		signal expoente1, expoente2 : std_logic_vector (7 downto 0);
		
		signal mantissa1, mantissa2: std_logic_vector (22 downto 0);
		
		signal rl, sl, vaium2, not_vaium2, out_and1, out_and2, not_out_and2, not_sel_sub, sel_sub :std_logic;
		signal will_round: std_logic;
		signal will_round_extd: std_logic_vector(22 downto 0);

		signal out_sub, out_compl3: std_logic_vector(7 downto 0);
	
		signal count: std_logic_vector(7 downto 0);
		signal vaium2_extd, out_and2_extd, out_fix_exp: std_logic_vector(7 downto 0);


		signal out_mux1, out_compl1: std_logic_vector(22 downto 0);
	   	signal out_sum2: std_logic_vector(22 downto 0);
		signal out_compl2: std_logic_vector(22 downto 0);
		signal out_shift2: std_logic_vector(22 downto 0);
	   	signal out_shift3, out_shift4: std_logic_vector(22 downto 0);

		-- Signals stage 1
		signal EX1_g, EX1_r, EX1_s, EX1_out_comp, EX1_out_mux4: std_logic;
		signal EX1_out_shift1, EX1_out_mux2:std_logic_vector(22 downto 0);
		signal EX1_out_mux3: std_logic_vector(7 downto 0);

		-- Signals stage 2
		signal EX2_out_comp, EX2_vaium1, EX2_g, EX2_r, EX2_s, EX2_out_mux4: std_logic;
		signal EX2_out_mux3: std_logic_vector(7 downto 0);
		signal EX2_out_sum1, EX2_out_shift1, EX2_out_mux2: std_logic_vector(22 downto 0);

		-- Signals stage 3
		signal EX3_out_comp, EX3_vaium1, EX3_g, EX3_r, EX3_s, EX3_out_mux4: std_logic;
		signal EX3_out_mux3: std_logic_vector(7 downto 0);
		signal EX3_out_sum1, EX3_out_shift1, EX3_out_mux2: std_logic_vector(22 downto 0);

	begin

		-----------------------------------------------------------------------------
		-- First Stage: EX1
		-----------------------------------------------------------------------------

		-- Unpack float1
		sinal1 <= float1(31);
		expoente1 <= float1(30 downto 23);
		mantissa1 <= float1(22 downto 0);

		-- Unpack float2
		sinal2 <= float2(31);
		expoente2 <= float2(30 downto 23);
		mantissa2 <= float2(22 downto 0);

		-- Subtract expoents 
		Sub: entity work.sub_exp port map(expoente1, expoente2, out_sub);

		-- Comp: entity work.comp port map(sinal1, sinal2, out_comp);
		EX1_out_comp <= sinal1 xor sinal2;

		sel_sub <= out_sub(7);

		Mux4: entity work.mux1  port map(sinal1, sinal2, EX1_out_comp, EX1_out_mux4);

		Mux1: entity work.mux23 port map(mantissa1, mantissa2, sel_sub, out_mux1);

		Compl1: entity work.compl2_23 port map(out_mux1, EX1_out_comp, out_compl1);

		Compl3: entity work.compl2_8 port map(out_sub, sel_sub, out_compl3);

		Shift_Mantissa: entity work.shift_mantissa port map(out_compl1, out_compl3, EX1_g, EX1_r, EX1_s, EX1_out_shift1);

		not_sel_sub <= not sel_sub;

		Mux2: entity work.mux23 port map(mantissa1, mantissa1, not_sel_sub, EX1_out_mux2);
		Mux3: entity work.mux8 port map(expoente1, expoente2, not_sel_sub, EX1_out_mux3);

		
		EX1: process(clk)
		begin
			if rising_edge(clk) then
				if rst = '1' then
					
					EX2_g <= '0';
					EX2_s <= '0';
					EX2_r <= '0';
					EX2_out_comp <= '0';
					EX2_out_shift1 <= (others => '0');
					EX2_out_mux2 <= (others => '0');
					EX2_out_mux3 <= (others => '0');
					EX2_out_mux4 <= '0';

				else

					EX2_g <= EX1_g;
					EX2_s <= EX1_s;
					EX2_r <= EX1_r;
					EX2_out_comp <= EX1_out_comp;
					EX2_out_shift1 <= EX1_out_shift1;
					EX2_out_mux2 <= EX1_out_mux2;
					EX2_out_mux3 <= EX1_out_mux3;
					EX2_out_mux4 <= EX1_out_mux4;
	
				end if;
			end if;
		end process;

		-----------------------------------------------------------------------------
		-- Second Stage: EX2
		-----------------------------------------------------------------------------

		Sum1: entity work.sum port map(EX2_out_shift1, EX2_out_mux2, EX2_out_sum1, EX2_vaium1);

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
					EX3_out_shift1 <= (others => '0');
					EX3_out_mux2 <= (others => '0');
					EX3_out_mux4 <= '0';

				else

					EX3_out_comp <= EX2_out_comp;
					EX3_vaium1 <= EX2_vaium1;
					EX3_g <= EX2_g;
					EX3_s <= EX2_s;
					EX3_r <= EX2_r;
					EX3_out_sum1 <= EX2_out_sum1;
					EX3_out_shift1 <= EX2_out_shift1;
					EX3_out_mux2 <= EX2_out_mux2;
					EX3_out_mux3 <= EX2_out_mux3;
					EX3_out_mux4 <= EX2_out_mux4;

				end if;
			end if;
		end process;

		-----------------------------------------------------------------------------
		-- Third Stage: EX3
		-----------------------------------------------------------------------------
		
		out_and1 <= EX3_vaium1 and (EX3_out_sum1(22) and (not EX3_out_comp));
		out_and2 <= EX3_vaium1 and EX3_out_comp;

		Compl2: entity work.compl2_23 port map(EX3_out_sum1, out_and1, out_compl2);

		not_out_and2 <= not out_and2;
		not_vaium2 <= not vaium2;

		Shift_Sum_Right: entity work.shift_one_right port map(out_compl2, out_and2, out_shift2);
		Shift_Sum_Left: entity work.shift_sum_left port map(out_shift2, EX3_g, out_shift3, count);
		
		Shift_One_Right: entity work.shift_one_right port map(out_sum2, not_vaium2, out_shift4);

		Round: entity work.round port map(out_compl2(0), rl, sl, EX3_out_mux4, will_round);

		will_round_extd <= "0000000000000000000000" & will_round;

		Sum2: entity work.sum port map(will_round_extd, EX3_out_mux2, EX3_out_sum1, EX3_vaium1);

		vaium2_extd <= "0000000" & vaium2;
		out_and2_extd <= "0000000" & out_and2;

		Fix_Exp: entity  work.fix_exp port map(EX3_out_mux3, out_and2_extd, vaium2_extd, count, out_fix_exp);

		Pandora: entity work.pandora port map(out_shift3(0), EX3_r, EX3_s, EX3_g, out_and2, count, rl, sl);		

		result <= EX3_out_mux4 & out_fix_exp & out_shift4; 

	end somador_arch;
