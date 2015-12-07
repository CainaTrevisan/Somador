library ieee;
	use ieee.std_logic_1164.all;
	use ieee.numeric_std.all;
	entity somador is
		port (  AB_in: in std_logic_vector(31 downto 0);
				clk, rst, wt_in: in std_logic;
				sela, selb, selc: in std_logic;
				result: out std_logic_vector(31 downto 0);
				pronto, wt_out: out std_logic);
	end somador;

	architecture somador_arch of somador is
			
		signal float1, float2: std_logic_vector(31 downto 0);

		-----------------------------------------------------------------------
		-- Signals stage 1
		-----------------------------------------------------------------------
		signal EX1_stalled_selb: std_logic;
		
		-----------------------------------------------------------------------
		-- Signals stage 2
		-----------------------------------------------------------------------

		-- expoentes
		signal EX2_expoente1, EX2_expoente2 : std_logic_vector (8 downto 0);
		signal EX2_exp_preliminar : std_logic_vector(8 downto 0);
		signal EX2_out_compl3 : std_logic_vector(8 downto 0);
		signal EX2_dif_exp : std_logic_vector(8 downto 0);

		-- mantissas
		signal EX2_mantissa1_init, EX2_mantissa2_init : std_logic_vector (22 downto 0);
		signal EX2_mantissa1, EX2_mantissa2 : std_logic_vector (23 downto 0);
		signal EX2_out_mux2 :std_logic_vector(23 downto 0);
		signal EX2_mantissa_esq : std_logic_vector(23 downto 0);
		signal EX2_out_compl1, EX2_out_shift1 : std_logic_vector(24 downto 0);

		-- bits de arredondamento
		signal  EX2_g, EX2_r, EX2_s : std_logic;

		-- flags e enables
		signal EX2_sel_operando, EX2_sinal_escolhido, EX2_and_sel4 : std_logic;
		signal denormal, EX2_both_denormal, EX2_denormal1, EX2_denormal2 : std_logic;
		signal EX2_mantissa1_zerada, EX2_mantissa2_zerada : std_logic; 
		signal EX2_NaN, EX2_Inf, EX2_sinal, EX2_zero1, EX2_zero2, EX2_menos_zero : std_logic;
		signal EX2_or_dif_exp, EX2_sel_sub : std_logic;
 
		-----------------------------------------------------------------------
		-- Signals stage 3
		-----------------------------------------------------------------------
		
		-- expoentes
		signal EX3_exp_preliminar : std_logic_vector(8 downto 0);

		-- mantissas
		signal EX3_out_mux2 : std_logic_vector(23 downto 0);
		signal EX3_out_mux2_ext : std_logic_vector(24 downto 0);
		signal EX3_out_sum1, EX3_out_shift1 : std_logic_vector(24 downto 0);

		-- bits de arredondamento
		signal EX3_g, EX3_r, EX3_s : std_logic;

		-- flags e enables
		signal EX2_sinal1, EX2_sinal2 : std_logic;
		signal  EX3_menos_zero, EX3_both_denormal, EX3_denormal1, EX3_denormal2 : std_logic;
		signal EX3_NaN, EX3_Inf, EX3_sinal, EX3_zero1, EX3_zero2 : std_logic;
		signal EX3_or_dif_exp, EX3_sel_operando, EX3_vaium1, EX3_out_mux4 : std_logic;

		-----------------------------------------------------------------------
		-- Signals stage 4
		-----------------------------------------------------------------------
		
		-- expoentes
		signal EX4_exp_preliminar : std_logic_vector(8 downto 0);
		signal EX4_out_fix_exp, EX4_exp_final : std_logic_vector(7 downto 0);

		signal EX4_vaium2_extd, EX4_out_and2_extd : std_logic_vector(7 downto 0);

		-- mantissas 
		signal EX4_out_mux2 : std_logic_vector(23 downto 0);
		signal EX4_out_sum1: std_logic_vector(24 downto 0);
	   	signal EX4_out_sum2: std_logic_vector(24 downto 0);
		signal EX4_out_compl_soma: std_logic_vector(24 downto 0);
		signal EX4_out_shift2: std_logic_vector(24 downto 0);
	   	signal EX4_out_shift3, EX4_out_shift4: std_logic_vector(24 downto 0);

		signal EX4_will_round_extd: std_logic_vector(24 downto 0);

		-- bits de arredondamento
		signal EX4_g, EX4_r, EX4_rl, EX4_s, EX4_sl : std_logic;
		signal EX4_will_round: std_logic;
		signal EX4_lst_sign: std_logic;

		-- shifts
		signal EX4_count: std_logic_vector(7 downto 0);

		-- flags e enables
		signal EX4_vaium2, EX4_out_and1, EX4_out_and2, not_EX4_out_and2:std_logic;
		signal EX4_menos_zero, EX4_both_denormal, EX4_denormal1, EX4_denormal2: std_logic;
		signal EX4_sel_operando, EX4_vaium1, EX4_out_mux4 : std_logic;
		signal inf_overflow, inf_final: std_logic;
		signal EX4_mantissa_zerada, EX4_vai_dar_zero: std_logic;
		signal EX4_sinal, EX4_sinal_corrigido, EX4_zero1, EX4_zero2: std_logic;
		signal EX4_or_dif_exp: std_logic;
		signal EX4_flag_denormal, EX4_NaN, EX4_Inf : std_logic;

	begin

		-----------------------------------------------------------------------------
		-- First Stage: EX1
		-----------------------------------------------------------------------------
		reg_a: process(rst, sela, AB_in)
		begin
			if rst = '1' then
				float1 <= (others => '0');	
			elsif (sela = '1') then
				float1 <= AB_in;
			end if;
			
		end process;

		reg_b: process(rst, selb, AB_in)
			begin
				if rst = '1' then
					float2 <= (others => '0');
				elsif (selb = '1') then
					float2 <= AB_in;
	
				end if;
		end process;

		stall_selb: process(clk, rst, selb)
		begin
			if rising_edge(clk) then
				if rst = '1' then
					EX1_stalled_selb <= '0';
				else
					EX1_stalled_selb <= selb;
				end if;	
			end if;	
		end process;	

		-----------------------------------------------------------------------------
		-- Second Stage: EX2
		-----------------------------------------------------------------------------

		EX2_denormal1 <= '1' when (float1(30 downto 23) = x"00") else '0'; 
		EX2_denormal2 <= '1' when (float2(30 downto 23) = x"00") else '0'; 

		EX2_both_denormal <= EX2_denormal1 and EX2_denormal2; 

		EX2_mantissa1_zerada <= 
				'1' when (float1(22 downto 0) = b"00000000000000000000000") else '0'; 

		EX2_mantissa2_zerada <= 
				'1' when (float2(22 downto 0) = b"00000000000000000000000") else '0'; 
	

		-- Unpack float1
		EX2_sinal1 <= float1(31);

        EX2_expoente1 <= '0' & float1(30 downto 23)
                when (EX2_denormal1 = '0'
                        or
                        ( (EX2_denormal1 = '1' and EX2_mantissa1_zerada = '1')
                            or
                          (EX2_denormal2 = '1' and EX2_mantissa2_zerada = '1')
                        )
                     )
                else b"000000001";

		EX2_mantissa1_init <= float1(22 downto 0);
		EX2_mantissa1 <= (not EX2_denormal1) & EX2_mantissa1_init;

		-- Unpack float2
		EX2_sinal2 <= float2(31);

        EX2_expoente2 <= '0' & float2(30 downto 23)
                when (EX2_denormal2 = '0'
                        or
                        ( (EX2_denormal2 = '1' and EX2_mantissa2_zerada = '1')
                            or
                          (EX2_denormal1 = '1' and EX2_mantissa1_zerada = '1')
                        )
                      )
                else b"000000001";

		EX2_mantissa2_init <= float2(22 downto 0);
		EX2_mantissa2 <= (not EX2_denormal2) & EX2_mantissa2_init;

		EX2_zero1 <= '1' when (EX2_denormal1='1' and EX2_mantissa1_zerada='1') else '0'; 

		EX2_zero2 <= '1' when (EX2_denormal2='1' and EX2_mantissa2_zerada='1') else '0'; 

		-- Subtract exponents 
		Sub: entity work.sub_exp port map(EX2_expoente1, EX2_expoente2, EX2_dif_exp);

		EX2_or_dif_exp <= '0' when (EX2_dif_exp = b"000000000") else '1';

		EX2_sel_operando <= EX2_sinal1 xor EX2_sinal2;

		EX2_sel_sub <= EX2_dif_exp(8);

		EX2_and_sel4 <= EX2_sel_operando and EX2_sel_sub;	

		EX2_sinal_escolhido <= EX2_sinal1 when(EX2_and_sel4='0') else EX2_sinal2;

		marido_de_aluguel: entity work.marido_de_aluguel 
				port map (clk, EX2_sinal1, EX2_sinal2, float1(30 downto 23),
						  float2(30 downto 23), EX2_mantissa1_init, EX2_mantissa2_init, 
						  EX2_sinal_escolhido, EX2_Inf, EX2_NaN, EX2_sinal, EX2_menos_zero);

		Mux1: entity work.mux24 
				port map(EX2_mantissa1, EX2_mantissa2, EX2_sel_sub, EX2_mantissa_esq);

		Compl_dif_exp: entity work.compl2_8 
				port map(EX2_dif_exp, EX2_sel_sub, EX2_out_compl3); -- n shift da mantissa

		Compl_mantissa_in: entity work.compl2_24 
				port map(EX2_mantissa_esq, EX2_sel_operando, EX2_out_compl1);

		Shift_Maitissa: entity work.shift_mantissa 
				port map(EX2_out_compl1, EX2_out_compl3, EX2_g, EX2_r, EX2_s, EX2_out_shift1);

		Mux2: entity work.mux24 
				port map(EX2_mantissa2, EX2_mantissa1, EX2_sel_sub, EX2_out_mux2);

		Mux3: entity work.mux8 
				port map(EX2_expoente1, EX2_expoente2, EX2_sel_sub, EX2_exp_preliminar);

		
		EX1: process(clk)
		begin
			if rising_edge(clk) then
				if rst = '1' then
					
					EX3_g <= '0';
					EX3_s <= '0';
					EX3_r <= '0';
					EX3_sel_operando <= '0';
					EX3_out_shift1 <= (others => '0');
					EX3_out_mux2 <= (others => '0');
					EX3_exp_preliminar <= (others => '0');
					EX3_sinal <= '0';
					EX3_or_dif_exp <= '0';
					EX3_NaN <= '0';
					EX3_Inf <= '0';
					EX3_menos_zero <= '0';
					EX3_both_denormal <= '0';
					EX3_denormal1 <= '0';
					EX3_denormal2 <= '0';
					EX3_zero1 <= '0';
					EX3_zero2 <= '0';

				else
					EX3_g <= EX2_g;
					EX3_s <= EX2_s;
					EX3_r <= EX2_r;
					EX3_sel_operando <= EX2_sel_operando;
					EX3_out_shift1 <= EX2_out_shift1;
					EX3_out_mux2 <= EX2_out_mux2;
					EX3_exp_preliminar <= EX2_exp_preliminar;
					EX3_sinal <= EX2_sinal;
					EX3_or_dif_exp <= EX2_or_dif_exp;
					EX3_NaN <= EX2_NaN;
					EX3_Inf <= EX2_Inf;
					EX3_menos_zero <= EX2_menos_zero;
					EX3_both_denormal <= EX2_both_denormal;
					EX3_denormal1 <= EX2_denormal1;
					EX3_denormal2 <= EX2_denormal2;
					EX3_zero1 <= EX2_zero1;
					EX3_zero2 <= EX2_zero2;
	
				end if;
			end if;
		end process;

		-----------------------------------------------------------------------------
		-- Third Stage: EX3
		-----------------------------------------------------------------------------

		EX3_out_mux2_ext <= '0' & EX3_out_mux2;
		Sum1: entity work.sum 
				port map(EX3_out_shift1, EX3_out_mux2_ext, EX3_out_sum1, EX3_vaium1);
		
			
		
		EX2: process(clk)
		begin
			if rising_edge(clk) then
				if rst = '1' then
					
					EX4_sel_operando <= '0';
					EX4_vaium1 <= '0';
					EX4_g <= '0';
					EX4_s <= '0';
					EX4_r <= '0';
					EX4_out_sum1 <= (others => '0');
					EX4_out_mux2 <= (others => '0');
					EX4_sinal <= '0';
					EX4_or_dif_exp <= '0';
					EX4_NaN <= '0';
					EX4_Inf <= '0';
					EX4_menos_zero <= '0';
					EX4_both_denormal <= '0';
					EX4_denormal1 <= '0';
					EX4_denormal2 <= '0';
					EX4_zero1 <= '0';
					EX4_zero2 <= '0';

				else

					EX4_sel_operando <= EX3_sel_operando;
					EX4_vaium1 <= EX3_vaium1;
					EX4_g <= EX3_g;
					EX4_s <= EX3_s;
					EX4_r <= EX3_r;
					EX4_out_sum1 <= EX3_out_sum1;
					EX4_out_mux2 <= EX3_out_mux2;
					EX4_exp_preliminar <= EX3_exp_preliminar;
					EX4_sinal <= EX3_sinal;
					EX4_or_dif_exp <= EX3_or_dif_exp;
					EX4_NaN <= EX3_NaN;
					EX4_Inf <= EX3_Inf;
					EX4_menos_zero <= EX3_menos_zero;
					EX4_both_denormal <= EX3_both_denormal;
					EX4_denormal1 <= EX3_denormal1;
					EX4_denormal2 <= EX3_denormal2;
					EX4_zero1 <= EX3_zero1;
					EX4_zero2 <= EX3_zero2;

				end if;
			end if;
		end process;

		-----------------------------------------------------------------------------
		-- Fourth Stage: EX4
		-----------------------------------------------------------------------------

		EX4_vai_dar_zero <= '1' 
				when ( (EX4_out_sum1&EX4_g&EX4_r&EX4_s) = "0000000000000000000000000000") 
				else '0';

		denormal <= '1' when (EX4_exp_preliminar = b"00000000") else '0';

		EX4_mantissa_zerada <= '1' when (EX4_out_shift2 = b"0000000000000000000000000") else '0';

		EX4_out_and1 <= EX4_vaium1 and EX4_sel_operando; 
		EX4_out_and2 <= EX4_vaium1 and (not EX4_sel_operando);

		Compl_mantissa_out: entity work.compl2_25 
				port map(EX4_out_sum1, EX4_out_and1, EX4_out_compl_soma);

		EX4_sinal_corrigido <= EX4_sinal 
			when ((EX4_denormal1='1' 
				or EX4_denormal2='1') 
				and (EX4_zero1='1' 
				or EX4_zero2='1')) 
			else (EX4_sinal xor EX4_out_and1);

		not_EX4_out_and2 <= not EX4_out_and2;
		
		EX4_lst_sign <= EX4_out_compl_soma(0);

		Shift_Sum_Right: entity work.shift_sum_right 
				port map(EX4_out_compl_soma, EX4_out_and2, EX4_out_shift2);

		Shift_Sum_Left: entity work.shift_sum_left 
				port map(EX4_out_and2, EX4_exp_preliminar, EX4_both_denormal, denormal, 
						 EX4_mantissa_zerada, EX4_out_shift2, not_EX4_out_and2, EX4_g, 
						 EX4_out_shift3, EX4_count, EX4_flag_denormal);

		Pandora: entity work.pandora 
				port map(EX4_lst_sign, EX4_r, EX4_s, EX4_g, EX4_out_and2, EX4_count, EX4_rl, EX4_sl);		
		Round: entity work.round 
				port map(EX4_out_shift3(0), EX4_rl, EX4_sl, EX4_out_mux4, EX4_will_round);

		EX4_will_round_extd <= "000000000000000000000000" & EX4_will_round;

		Sum2: entity work.sum 
				port map(EX4_will_round_extd, EX4_out_shift3, EX4_out_sum2, EX4_vaium2);

		Shift_One_Right: entity work.shift_one_right 
				port map(EX4_out_sum2, EX4_vaium2, EX4_out_shift4);

		EX4_vaium2_extd <= "0000000" & EX4_vaium2;

		EX4_out_and2_extd <= "0000000" & EX4_out_and2;

		Fix_Exp: entity  work.fix_exp 
				port map(EX4_exp_preliminar, EX4_out_and2_extd, EX4_vaium2_extd, 
						 EX4_count, EX4_out_fix_exp, EX4_flag_denormal, EX4_menos_zero, 
						 EX4_out_sum1(23));

		inf_overflow<=(
		EX4_exp_preliminar(7) and 
		EX4_exp_preliminar(6) and 
		EX4_exp_preliminar(5) and 
		EX4_exp_preliminar(4) and 
		EX4_exp_preliminar(3) and --exp="111111xx"
		EX4_exp_preliminar(2)
		)
		and
		(
			(
				(EX4_exp_preliminar(1) and not (EX2_exp_preliminar(0))) -- exp="11111110"
		 		and (EX4_out_and2 or EX4_vaium2)
			)
			or
			(
				((not EX4_exp_preliminar(1)) and EX4_exp_preliminar(0))
				and (EX4_out_and2 and EX4_vaium2)
			)
		);

		inf_final <= inf_overflow or EX4_Inf;

		EX4_exp_final <= b"00000000" 
				when (EX4_out_shift4(23) = '0') else EX4_out_fix_exp(7 downto 0); 

		Trata_Excp: entity work.trata_excp 
				port map(EX4_exp_final, EX4_out_shift4(22 downto 0), EX4_NaN, inf_final, 
						 EX4_sinal_corrigido, EX4_vai_dar_zero, EX4_or_dif_exp, 
						 EX4_sel_operando, EX4_menos_zero, result);

	end somador_arch;
