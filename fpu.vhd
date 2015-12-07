-- ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
--  cMIPS, a VHDL model of the classical five stage MIPS pipeline.
--  Copyright (C) 2015  Joao Manoel Pampanini Filho & Roberto Andre Hexsel
--
--  This program is free software: you can redistribute it and/or modify
--  it under the terms of the GNU General Public License as published by
--  the Free Software Foundation, version 3.
--
--  This program is distributed in the hope that it will be useful,
--  but WITHOUT ANY WARRANTY; without even the implied warranty of
--  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
--  GNU General Public License for more details.
--
--  You should have received a copy of the GNU General Public License
--  along with this program.  If not, see <http://www.gnu.org/licenses/>.
-- ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

library IEEE;
use IEEE.std_logic_1164.all;
use IEEE.numeric_std.all;
use work.p_wires.all;

entity special_values is
  port (in_a,in_b         : in  std_logic_vector(30 downto 0);
        type_A,type_b     : out FP_type;
        denormA,denormB   : out std_logic);
end special_values;

architecture estrutural of special_values is

  -- type_A= 11    A=0.0
  -- type_A= 01    infinito
  -- type_A= 10    NaN
  -- type_A= 00    A é numero bom
  -- type FP_type is fp_is_good, fp_is_inf, fp_is_NaN, fp_is_zero;

  constant mant_all_zeroes : reg23 := (others => '0');
  constant exp_all_zeroes  : reg8  := (others => '0');
  constant exp_all_ones    : reg8  := (others => '1');

begin

  U_check_A: process(in_a)
    variable mant_is_zero, exp_is_zero, exp_is_255 : boolean;
  begin
    mant_is_zero := (in_a(22 downto  0) = mant_all_zeroes);
    exp_is_zero  := (in_a(30 downto 23) = exp_all_zeroes);
    exp_is_255   := (in_a(30 downto 23) = exp_all_ones);

    if exp_is_zero and mant_is_zero then
      type_A <= fp_is_zero;
    elsif exp_is_255 and mant_is_zero then
      type_A <= fp_is_inf;
    elsif exp_is_255 and not(mant_is_zero) then
      type_A <= fp_is_nan;
    else
      type_A <= fp_is_good;
    end if;

    if exp_is_zero then
      denormA <= '0';
    else
      denormA <= '1';
    end if;

  end process U_check_A;


  U_check_B: process(in_b)
    variable mant_is_zero, exp_is_zero, exp_is_255 : boolean;
  begin
    mant_is_zero := (in_b(22 downto  0) = mant_all_zeroes);
    exp_is_zero  := (in_b(30 downto 23) = exp_all_zeroes);
    exp_is_255   := (in_b(30 downto 23) = exp_all_ones);

    if exp_is_zero and mant_is_zero then
      type_B <= fp_is_zero;
    elsif exp_is_255 and mant_is_zero then
      type_B <= fp_is_inf;
    elsif exp_is_255 and not(mant_is_zero) then
      type_B <= fp_is_nan;
    else
      type_B <= fp_is_good;
    end if;

    if exp_is_zero then
      denormB <= '0';
    else
      denormB <= '1';
    end if;

  end process U_check_B;

    -- type_A <= b"11" when to_integer(unsigned(in_a(30 downto 23))) = 0 AND to_integer(unsigned(in_a(22 downto 0))) = 0
    -- else b"01" when to_integer(unsigned(in_a(30 downto 23))) = 255 AND to_integer(unsigned(in_a(22 downto 0))) = 0
    -- else b"10" when to_integer(unsigned(in_a(30 downto 23))) = 255 AND to_integer(unsigned(in_a(22 downto 0))) /= 0
    -- else b"00";

    -- type_b <= b"11" when to_integer(unsigned(in_b(30 downto 23))) = 0 AND to_integer(unsigned(in_b(22 downto 0))) = 0
    -- else b"01" when to_integer(unsigned(in_b(30 downto 23))) = 255 AND to_integer(unsigned(in_b(22 downto 0))) = 0
    -- else b"10" when to_integer(unsigned(in_b(30 downto 23))) = 255 AND to_integer(unsigned(in_b(22 downto 0))) /= 0
    -- else b"00";

  -- denormA <= '0' when to_integer(unsigned(in_a(30 downto 23))) = 0 else '1';
  -- denormB <= '0' when to_integer(unsigned(in_b(30 downto 23))) = 0 else '1';

end estrutural;

--+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
library IEEE;
use IEEE.std_logic_1164.all;
use IEEE.numeric_std.all;
use work.p_wires.all;

entity data_check_mult is
    port(type_A,type_B : in  FP_type;
         sig_A,sig_B   : in  std_logic;
         sig_out       : out std_logic;
         exp_in        : in  std_logic_vector ( 7 downto 0);
         fra_in        : in  std_logic_vector (22 downto 0);
         exp_out       : out std_logic_vector ( 7 downto 0);
         fra_out       : out std_logic_vector (22 downto 0));
end data_check_mult;

architecture estrutural of data_check_mult is
begin

  check : process(type_A,type_B, exp_in,fra_in)
    variable exp_p : std_logic_vector ( 7 downto 0);
    variable fra_p : std_logic_vector (22 downto 0);
  begin

    -- if (type_A = "10" OR type_B = "10" ) then
      --   exp_p := (OTHERS => '1');
      -- fra_p := (OTHERS => '1');
    -- elsif ( (type_A = "01" AND type_B = "11") OR (type_A = "11" AND type_B = "01") ) then
      -- exp_p := (OTHERS => '1');
      -- fra_p := (OTHERS => '1');
    -- elsif (type_A = "01" OR type_B = "01") then
      -- exp_p := (OTHERS => '1');
      -- fra_p := (OTHERS => '0');
    -- elsif (type_A = "11" OR type_B = "11") then
      -- exp_p := (OTHERS => '0');
      -- fra_p := (OTHERS => '0');
    -- else
      -- exp_p := exp_in;
      -- fra_p := fra_in;
    -- end if;

    case type_A is
      when fp_is_NaN =>                 -- 10
        exp_p := (OTHERS => '1');
        fra_p := (OTHERS => '1');
      when fp_is_inf =>                 -- 01
        exp_p := (OTHERS => '1');
        if type_B = fp_is_zero then     -- 11
          fra_p := (OTHERS => '1');
        else
          fra_p := (OTHERS => '0');
        end if;
      when fp_is_zero =>                -- 11
        if type_B = fp_is_inf then      -- 01
          exp_p := (OTHERS => '1');
          fra_p := (OTHERS => '1');
        else
          exp_p := (OTHERS => '0');
          fra_p := (OTHERS => '0');
        end if;
      when others =>
        case type_B is
          when fp_is_NaN =>             -- 10
            exp_p := (OTHERS => '1');
            fra_p := (OTHERS => '1');
          when fp_is_zero =>            -- 10
            exp_p := (OTHERS => '0');
            fra_p := (OTHERS => '0');
          when others =>
            exp_p := exp_in;
            fra_p := fra_in;
        end case;
    end case;

    exp_out <= exp_p ;
    fra_out <= fra_p ;
  end process;

  check_sig : process(type_A,type_B, sig_A,sig_B)
    variable sig_p : std_logic;
  begin
    if (type_A = fp_is_NaN OR type_B = fp_is_NaN ) then
      sig_p := '0';
    elsif ( (type_A = fp_is_inf  AND type_B = fp_is_zero) OR
            (type_A = fp_is_zero AND type_B = fp_is_inf ) ) then
      sig_p := '0';
    else
      sig_p := sig_A XOR sig_B;
    end if;

    sig_out <= sig_p;
  end process check_sig;

end estrutural;


-- ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
-- MULT_FLOAT
-- ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
library IEEE;
use IEEE.std_logic_1164.all;
use IEEE.numeric_std.all;
use work.p_wires.all;

entity mult32float is
  port(AB_in          : in  std_logic_vector(31 downto 0);
       rel,rst,wt_in  : in  std_logic;
       sela,selb,selc : in  std_logic;
       prod           : out std_logic_vector(31 downto 0);
       pronto,wt_out  : out std_logic);
end mult32float;

architecture estrutural of mult32float is

  component special_values is
    port (in_a,in_b         : in  std_logic_vector;
          type_A,type_b     : out FP_type;
          denormA,denormB   : out std_logic);
  end component special_values;

  component data_check_mult is
    port (type_A,type_B : in  FP_type;
          sig_A,sig_B   : in  std_logic;
          sig_out       : out std_logic;
          exp_in        : in  std_logic_vector;
          fra_in        : in  std_logic_vector;
          exp_out       : out std_logic_vector;
          fra_out       : out std_logic_vector);
  end component data_check_mult;


signal vlid_stg0,wt_stg0,flag,denormA,denormB: std_logic;
signal type_A,type_B : FP_type;
signal expA,expB,exp_ab,exp_p,desloc : std_logic_vector(8 downto 0);
signal in_A,in_B : std_logic_vector(31 downto 0);

signal vlid_stg1,wt_stg1,sign_A,sign_B : std_logic;
signal type_A2,type_B2 : FP_type;
signal deloc2 : std_logic_vector(4 downto 0);
signal exp_stg1 : std_logic_vector(7 downto 0);
signal frac_A,frac_B : std_logic_vector(23 downto 0);
signal frac_p : std_logic_vector(47 downto 0);

signal vlid_stg2,wt_stg2,sign_A2,sign_B2,sign_f : std_logic;
signal type_A3,type_B3 : FP_type;
signal deloc3,deloc4 : std_logic_vector(4 downto 0);
signal exp_stg2,exp_stg3,exp_f : std_logic_vector(7 downto 0);
signal frac_f,mant : std_logic_vector(22 downto 0);
signal frac_normed : std_logic_vector(23 downto 0);
signal frac_rounded : std_logic_vector(25 downto 0);
signal signR : std_logic_vector(47 downto 0);

signal w_intra,w_pronto: std_logic;

begin

  -- ENTRADA
  stg0_start: process(rel)
  begin
    if rising_edge(rel) then
      if (rst = '0' or (selA = selB and flag = '0')) then
        vlid_stg0 <= '0';
        wt_stg0   <= '0';
        flag      <= '0';
        in_A      <= x"00000000";
        in_B      <= x"00000000" ;
      elsif (selA = '1' and selB = '0') then
        vlid_stg0 <= '0';
        wt_stg0   <= '1';
        flag      <= '1';
        in_A      <= AB_in;
        in_B      <= x"00000000";
      elsif (selA = '0' and selB = '1') then
        vlid_stg0 <= '1';
        wt_stg0   <= '1';
        flag      <= '0';
        in_A      <= in_A;
        in_B      <= AB_in;
      end if;
    end if;
  end process;

  s_cases : special_values
    port map (in_A(30 downto 0),in_B(30 downto 0),
              type_A,type_B, denormA,denormB);

  -- separa expoentes da entrada
  expA <= '0' & in_A(30 downto 23) when denormA = '1' else b"000000001";
  -- e agenta para a soma
  expB <= '0' & in_B(30 downto 23) when denormB = '1' else b"000000001";

  -- soma dos expoentes
  exp_ab <=  std_logic_vector(signed(expA) + signed(expB)); -- soma com a bias

  exp_p <= '0' & x"ff" when exp_ab >= b"011111111" else
           '0' & x"00" when exp_ab <= b"001111111" else
           std_logic_vector(signed(exp_ab) + signed'("110000001"));

  desloc <= b"000000000" when exp_ab > b"001111111" else
            std_logic_vector(signed'("010000000") - signed(exp_ab));

  -- MULT
  stg1_Mult: process(rel)
  begin
    if rising_edge(rel) then
      if (rst = '0' or vlid_stg0 = '0') then
        vlid_stg1   <= '0'            ;
        wt_stg1     <= '0'            ;
        type_A2     <= fp_is_good;
        type_B2     <= fp_is_good;
        sign_A      <= '0'            ;
        sign_B      <= '0'            ;
        deloc2      <= (OTHERS => '0');
        exp_stg1    <= (OTHERS => '0');
        frac_A      <= (OTHERS => '0');
        frac_B      <= (OTHERS => '0');
      else
        vlid_stg1   <= '1';
        wt_stg1     <= wt_stg0;
        type_A2     <= type_A         ;
        type_B2     <= type_B         ;
        sign_A      <= in_A(31)       ;
        sign_B      <= in_B(31)       ;
        if (desloc < b"000011000") then
          deloc2 <= desloc(4 downto 0) ;
        else
          deloc2 <= "11000";
        end if;
        exp_stg1    <= exp_p(7 downto 0);
        frac_A       <= denormA & in_A(22 downto 0);
        frac_B       <= denormB & in_B(22 downto 0);
      end if;
    end if;
  end process;

  frac_p <= std_logic_vector(unsigned(frac_A) * unsigned(frac_B)) ;

  -- NORM e ROUND
  stg2_round: process(rel)
  begin
    if rising_edge(rel) then
      if (rst = '0' or vlid_stg1 = '0') then
        vlid_stg2   <= '0'     ;
        wt_stg2     <= '0'     ;
        type_A3     <= fp_is_good;
        type_B3     <= fp_is_good;
        sign_A2     <= '0'     ;
        sign_B2     <= '0'     ;
        deloc3      <= (OTHERS => '0');
        exp_stg2    <= (OTHERS => '0');
        signR       <= (OTHERS => '0');
      else
        vlid_stg2   <= '1'        ;
        wt_stg2     <= wt_stg1    ;
        type_A3     <= type_A2    ;
        type_B3     <= type_B2    ;
        sign_A2     <= sign_A     ;
        sign_B2     <= sign_B     ;
        deloc3      <= deloc2     ;
        exp_stg2    <= exp_stg1   ;
        signR       <= frac_p     ;
      end if;
    end if;
  end process;

  frac_rounded <= std_logic_vector(unsigned('0' & signR(46 downto 22)) + 1) WHEN signR(47) = '0'
               ELSE std_logic_vector(unsigned('0' & signR(47 downto 23)) + 1);

  deloc4 <=  std_logic_vector(unsigned(deloc3) - 1) when (signR(47) = '1' or frac_rounded(25) = '1') and deloc3 > b"00000" else deloc3;

  exp_stg3 <= x"00" when signR(47) = '0' and signR(46) = '0' else std_logic_vector(unsigned(exp_stg2) + 1) when (signR(47) = '1' or frac_rounded(25) = '1') and deloc3 = b"00000" else exp_stg2;

  frac_normed <= b"0000" & x"00000" when exp_stg3 = x"ff"
                 else std_logic_vector(unsigned(frac_rounded(25 downto 2)) srl to_integer(unsigned(deloc4))) when frac_rounded(25) = '1'
                 else std_logic_vector(unsigned(frac_rounded(24 downto 1)) srl to_integer(unsigned(deloc4)));

  mant <= frac_normed(22 downto 0);

  finish : data_check_mult
    port map (type_A3,type_B3,sign_A2,sign_B2,sign_f,exp_stg3,mant,exp_f,frac_f);

  -- Fim do processo
  stg3_final: process(rel)
  begin
    if rising_edge(rel) then
      if ( rst = '0' or
           (selc = '1' and  w_intra = '0' and vlid_stg2 = '0') ) then
        w_pronto <= '0';
        prod   <= x"00000000";
      elsif (selc = '0' and w_pronto = '1') then
        w_pronto <= '1';
      elsif (vlid_stg2 = '1') then
        w_pronto <= '1';
        prod   <= sign_f & exp_f  & frac_f;
      end if;
    end if;
  end process;

  pronto <= w_pronto;
  w_intra <= (wt_in or wt_stg0 or wt_stg1 or wt_stg2) and (not w_pronto) ;
  wt_out <= w_intra;

end estrutural;
-- ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
-- ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
-- ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
-- +++SOMADOR++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
-- ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
-- ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
-- ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
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
    port(a, b:  in  std_logic_vector(8 downto 0);
         result:    out std_logic_vector (8 DOWNTO 0));
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
    port (a     : in std_logic_vector(24 downto 0);
          n     : in std_logic_vector(8 downto 0);
          g, r, s   : out std_logic;
          result    : out std_logic_vector(24 downto 0));

    signal tmp, tmp2: std_logic_vector(49 downto 0);
    signal resto: std_logic_vector(24 downto 0) := (others => '0');
end shift_mantissa;

architecture sh_mant of shift_mantissa is
begin
    tmp <= a & "0000000000000000000000000";
    tmp2 <= to_stdlogicvector(to_bitvector(tmp) sra to_integer(unsigned(n)));
--  resto((to_integer(unsigned(n))-1) downto 0) <= a((to_integer(unsigned(n))-1) downto 0);
    resto <= tmp2(24 downto 0);
    result <= tmp2(49 downto 25);
--  resto <= to_stdlogicvector((to_bitvector(a) sll (24-(to_integer(unsigned(n))))));

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
         a  : in std_logic_vector(24 downto 0);
         en : in std_logic;
         g  : in std_logic;
         result : out std_logic_vector(24 downto 0);
         count  : out std_logic_vector(7 downto 0);
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

    Max_shift : process (exp_preliminar, enable)
    begin
        if (exp_preliminar = x"00") then
            max_shamt <= exp_preliminar;
        else
            max_shamt <= std_logic_vector(unsigned(exp_preliminar) - 1);
        end if;
    end process;

    P1: process(a, max_shamt)
    variable n : std_logic_vector(4 downto 0);
    variable f: std_logic;
    begin

        if ((a(23) = '1') or (max_shamt="000000000")) then n := "00000"; f := not(a(23));
        elsif a(22) = '1' or max_shamt="000000001" then n := "00001"; f := not(a(22));
        elsif (a(21) = '1' or  max_shamt="000000010") then n := "00010"; f := not(a(21));
        elsif (a(20) = '1' or  max_shamt="000000011") then n := "00011"; f := not(a(20));
        elsif (a(19) = '1' or  max_shamt="000000100") then n := "00100"; f := not(a(19));
        elsif (a(18) = '1' or  max_shamt="000000101") then n := "00101"; f := not(a(18));
        elsif (a(17) = '1' or  max_shamt="000000110") then n := "00110"; f := not(a(17));
        elsif (a(16) = '1' or  max_shamt="000000111") then n := "00111"; f := not(a(16));
        elsif (a(15) = '1' or  max_shamt="000001000") then n := "01000"; f := not(a(15));
        elsif (a(14) = '1' or  max_shamt="000001001") then n := "01001"; f := not(a(14));
        elsif (a(13) = '1' or  max_shamt="000001010") then n := "01010"; f := not(a(13));
        elsif (a(12) = '1' or  max_shamt="000001011") then n := "01011"; f := not(a(12));
        elsif (a(11) = '1' or  max_shamt="000001100") then n := "01100"; f := not(a(11));
        elsif (a(10) = '1' or  max_shamt="000001101") then n := "01101"; f := not(a(10));
        elsif (a(9) = '1' or  max_shamt="000001110") then n :=  "01110"; f := not(a(9));
        elsif (a(8) = '1' or  max_shamt="000001111") then n :=  "01111"; f := not(a(8));
        elsif (a(7) = '1' or  max_shamt="000010000") then n :=  "10000"; f := not(a(7));
        elsif (a(6) = '1' or  max_shamt="000010001") then n :=  "10001"; f := not(a(6));
        elsif (a(5) = '1' or  max_shamt="000010010") then n :=  "10010"; f := not(a(5));
        elsif (a(4) = '1' or  max_shamt="000010011") then n :=  "10011"; f := not(a(4));
        elsif (a(3) = '1' or  max_shamt="000010100") then n :=  "10100"; f := not(a(3));
        elsif (a(2) = '1' or  max_shamt="000010101") then n :=  "10101"; f := not(a(2));
        elsif (a(1) = '1' or  max_shamt="000010110") then n :=  "10110"; f := not(a(1));
        elsif (a(0) = '1' or  max_shamt="000010111") then n :=  "10111"; f := not(a(0));
        else n := "11000";
        end if;
        flag_denormal <= f;
        shamt_tmp <= n;

    end process P1;

    shamt <= shamt_tmp(4 downto 0);

    P2: process (a, shamt, en, denormal, both_denormal)
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

    end process P2;

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
    port (a     : in std_logic_vector(24 downto 0);
          enable    : in std_logic;
          result    : out std_logic_vector(24 downto 0));
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
    port (a     : in std_logic_vector(24 downto 0);
          enable    : in std_logic;
          result    : out std_logic_vector(24 downto 0));
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
    port (mst_sign  : in std_logic;
          round     : in std_logic;
          sticky    : in std_logic;
          sign      : in std_logic;
          result    : out std_logic);

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
    port (exp       : in std_logic_vector(8 downto 0);
          out_and   : in std_logic_vector(7 downto 0);
          vaium     : in std_logic_vector(7 downto 0);
          count     : in std_logic_vector(7 downto 0);
          result    : out std_logic_vector(7 downto 0);
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
--      tmp2 <= std_logic_vector((unsigned(tmp) - unsigned(count)));
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

        or_exp1 := exp1(7) or exp1(6) or exp1(5) or exp1(4) or exp1(3) or exp1(2) or exp1(1) or exp1(0);     -- exp1!="00000000"
        or_exp2 := exp2(7) or exp2(6) or exp2(5) or exp2(4) or exp2(3) or exp2(2) or exp2(1) or exp2(0);     -- exp2!="00000000"

        or_mantissa1 := mantissa1(22) or mantissa1(21) or mantissa1(20) or mantissa1(19)
        or mantissa1(18) or mantissa1(17) or mantissa1(16) or mantissa1(15) or mantissa1(14) or mantissa1(13) or mantissa1(12)
        or mantissa1(11) or mantissa1(10) or mantissa1(9) or mantissa1(8) or mantissa1(7) or mantissa1(6) or mantissa1(5)
        or mantissa1(4) or mantissa1(3) or mantissa1(2) or mantissa1(1) or mantissa1(0);             -- mantissa1!="00000000000000000000000"

        or_mantissa2 := mantissa2(22) or mantissa2(21) or mantissa2(20) or mantissa2(19)
        or mantissa2(18) or mantissa2(17) or mantissa2(16) or mantissa2(15) or mantissa2(14) or mantissa2(13) or mantissa2(12)
        or mantissa2(11) or mantissa2(10) or mantissa2(9) or mantissa2(8) or mantissa2(7) or mantissa2(6) or mantissa2(5)
        or mantissa2(4) or mantissa2(3) or mantissa2(2) or mantissa2(1) or mantissa2(0);             -- mantissa2!="00000000000000000000000"

        if (and_exp1='1') then                   -- operando1=NaN ou Infinito
            if (or_mantissa1='1') then           -- operando1==NaN
                nan <= '1';
                inf <= '0';
                if (and_exp2='1') then           -- operando2=NaN ou Infinito
                    if (or_mantissa2='1') then   -- operando2==NaN
                        sinaltmp<= sinal1 and sinal2;
                    else                 -- operando2==Infinito
                        sinaltmp<= sinal1;
                    end if;
                else                     -- operando2=Num ou 0
                    sinaltmp<= sinal1;
                end if;
            else                         -- operando1=Infinito
                if (and_exp2='1') then           -- operando2=NaN ou Infinito
                    if (or_mantissa2='1') then   -- operando2==NaN
                        nan <= '1';
                        inf <= '0';
                        sinaltmp<= sinal2;
                    else                 -- operando2==Infinito
                        sinaltmp<= sinal1 and sinal2;
                        nan <= sinal1 xor sinal2;
                        inf <= not (sinal1 xor sinal2);

                    end if;
                else                     -- operando2=Num ou 0
                    sinaltmp<= sinal1;
                    nan <= '0';
                    inf <= '1';
                end if;
            end if;
            menos_zero <= '0';
        else                             -- operando1=Num ou 0
            if ((or_mantissa1='0') and (or_exp1='0')) then   -- operando1==0
                if (and_exp2='1') then           -- operando2=NaN ou Infinito
                    sinaltmp<= sinal2;
                    if (or_mantissa2 = '1') then     -- operando2=NaN
                        nan <= '1';
                        inf <= '0';
                    else                 -- operando2=Infinito
                        nan <= '0';
                        inf <= '1';

                    end if;
                    menos_zero <= '0';
                else                     -- operando2=Num ou 0
                    nan <= '0';
                    inf <= '0';
                    if ((or_mantissa2='0') and (or_exp2='0')) then   --operando2==0
                        sinaltmp<= sinal1 and sinal2;
                        menos_zero <= sinal1 and sinal2;
                    else                 -- operando2==Num
                        sinaltmp<= sinal2;
                        menos_zero <= '0';
                    end if;
                end if;
            else                         -- operando1=Num
                if (and_exp2='1') then           -- operando2=NaN ou Infinito
                    sinaltmp<= sinal2;
                    if (or_mantissa2 = '1') then     -- operando2=NaN
                        nan <= '1';
                        inf <= '0';
                    else                 -- operando2=Infinito
                        nan <= '0';
                        inf <= '1';

                    end if;

                else                     -- operando2=Num ou 0
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
          nan       : in std_logic;
          inf       : in std_logic;
          sinal     : in std_logic;
          vai_dar_zero : in std_logic;
          or_dif_exp : in std_logic;
          sinais_diferentes : in std_logic;
          menos_zero : in std_logic;
          result    : out std_logic_vector(31 downto 0));

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

-- +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

library ieee;
    use ieee.std_logic_1164.all;
    use ieee.numeric_std.all;
    entity sum32float is
        port (  AB_in: in std_logic_vector(31 downto 0);
                clk, rst, wt_in: in std_logic;
                sela, selb, selc: in std_logic;
                result: out std_logic_vector(31 downto 0);
                pronto, wt_out: out std_logic);
    end sum32float;


    architecture somador_arch of sum32float is

        signal float1, float2: std_logic_vector(31 downto 0);

        -----------------------------------------------------------------------
        -- Signals stage 1
        -----------------------------------------------------------------------
        signal EX1_stalled_selb, wt1, vl1,flag: std_logic;

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
        signal EX2_sel_operando, EX2_sinal_escolhido, EX2_and_sel4,wt2, vl2 : std_logic;
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
        signal EX2_sinal1, EX2_sinal2,wt3, vl3 : std_logic;
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
        signal wtp,wti : std_logic;

        signal sum_f32 : std_logic_vector(31 downto 0);

    begin
-----------------------------------------------------------------------------
--------------------------------------------------------------------------------------
----------- First Stage: EX1 --------------------------------------------------------
--------------------------------------------------------------------------------------
-----------------------------------------------------------------------------
--      reg_a: process(rst, sela, selb, AB_in)
--      begin
--
--          if rst = '1' then
--              float1 <= (others => '0');
--              wt1 <= '0';
--              vl1 <= '0';
--          elsif (sela = '1') then
--              float1 <= AB_in;
--              wt1 <= '0';
--              vl1 <= '0';
--          end if;
--
--          if rst = '1' then
--              float2 <= (others => '0');
--              wt1 <= '0';
--              vl1 <= '0';
--          elsif (selb = '1') then
--              float2 <= AB_in;
--              wt1 <= '1';
--              vl1 <= '1';
--          end if;
--
--      end process;
--
--      stall_selb: process(clk, rst, selb)
--      begin
--          if rising_edge(clk) then
--              if rst = '1' then
--                  EX1_stalled_selb <= '0';
--              else
--                  EX1_stalled_selb <= selb;
--              end if;
--          end if;
--      end process;
stg0_start: process(clk)
  begin
    if rising_edge(clk) then
      if (rst = '0' or (selA = selB and flag = '0')) then
        vl1 <= '0';
        wt1   <= '0';
        flag      <= '0';
        float1      <= x"00000000";
        float2      <= x"00000000" ;
      elsif (selA = '1' and selB = '0') then
        vl1 <= '0';
        wt1   <= '1';
        flag      <= '1';
        float1      <= AB_in;
        float2      <= x"00000000";
      elsif (selA = '0' and selB = '1') then
        vl1 <= '1';
        wt1   <= '1';
        flag      <= '0';
        float2      <= AB_in;
      end if;
    end if;
  end process;
-----------------------------------------------------------------------------
-------------------------------------------------------------------------------------
---------- Second Stage: EX2 --------------------------------------------------------
-------------------------------------------------------------------------------------
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
                if rst = '0' then

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
                    wt2 <= '0';
                    vl2 <= '0';

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
                    wt2 <= wt1;
                    vl2 <= vl1;

                end if;
            end if;
        end process;
-----------------------------------------------------------------------------
-------------------------------------------------------------------------------------
---------- Third Stage: EX3 ---------------------------------------------------------
-------------------------------------------------------------------------------------
-----------------------------------------------------------------------------
        EX3_out_mux2_ext <= '0' & EX3_out_mux2;
        Sum1: entity work.sum
                port map(EX3_out_shift1, EX3_out_mux2_ext, EX3_out_sum1, EX3_vaium1);



        EX2: process(clk)
        begin
            if rising_edge(clk) then
                if rst = '0' then

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
                    wt3 <= '0';
                    vl3 <= '0';

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
                    wt3 <= wt2;
                    vl3 <= vl2;

                end if;
            end if;
        end process;
-----------------------------------------------------------------------------
-----------------------------------------------------------------------------------
---------- Fourth Stage: EX4-----------------------------------------------------
--------------------------------------------------------------------------------------
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
                         EX4_sel_operando, EX4_menos_zero, sum_f32);


        -- Fim do processo
          stg3_final: process(clk)
          begin
            if rising_edge(clk) then
              if ( rst = '0' or (selc = '1' and  wti = '0' and vl3 = '0') ) then
                wtp <= '0';
                result   <= x"00000000";
              elsif (selc = '0' and wtp = '1') then
                wtp <= '1';
              elsif (vl3 = '1') then
                wtp <= '1';
                result   <= sum_f32;
              end if;
            end if;
          end process;

          pronto <= wtp;
          wti <= (wt_in or wt1 or wt2 or wt3) and (not wtp) ;
          wt_out <= wti;

    end somador_arch;

-- ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
-- ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
-- ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

-- ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
-- FPU
-- ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
library IEEE;
use IEEE.std_logic_1164.all;
use work.p_wires.all;

entity FPU is
  port(rst      : in    std_logic;
       clk      : in    std_logic;
       sel      : in    std_logic;
       rdy      : out   std_logic;
       wr       : in    std_logic;
       addr     : in    reg4;
       data_inp : in    reg32;
       data_out : out   reg32);
end FPU;

architecture estrutural of FPU is

  component wait_states is
    generic (NUM_WAIT_STATES :integer);
    port(rst   : in  std_logic;
         clk     : in  std_logic;
         sel     : in  std_logic;         -- active in '0'
         waiting : out std_logic);        -- active in '1'
  end component wait_states;

  component mult32float is port(
    AB_in          : in  std_logic_vector;
    rel,rst,wt_in  : in  std_logic ;
    sela,selb,selc : in  std_logic ;
    prod           : out std_logic_vector;
    pronto,wt_out  : out std_logic);
  end component mult32float;

  component sum32float is port(
    AB_in            : in  std_logic_vector;
    clk,rst,wt_in    : in  std_logic ;
    sela,selb,selc   : in  std_logic ;
    result           : out std_logic_vector;
    pronto,wt_out    : out std_logic);
  end component sum32float;

  --component div32float is port(
  --AB_in          : in  std_logic_vector;
  --rel,rst,wt_in  : in  std_logic ;
  --sela,selb,selc : in  std_logic ;
  --prod           : out std_logic_vector;
  --pronto,wt_out  : out std_logic);
  --end component div32float;

  signal wt,wt0,pt0,selA_mul,selB_mul,selC_mul, wt_mul, wt_st0 : std_logic;
  signal    wt1,pt1,selA_sum,selB_sum,selC_sum, wt_sum, wt_st1 : std_logic;
  signal    wt2,pt2,selA_div,selB_div,selC_div, wt_div, wt_st2 : std_logic;
  signal RES_MUL,RES_SUM,RES_DIV               : std_logic_vector(31 DOWNTO 0);

begin

  U_Mult_float: mult32float
    port map (data_inp,clk,rst,'0',selA_mul,selB_mul,selC_mul,RES_MUL,pt0,wt0);

  U_Sum_float : sum32float
    port map (data_inp,clk,rst,'0',selA_sum,selB_sum,selC_sum,RES_SUM,pt1,wt1);

  -- U_Div_float : div32float
    -- port map (data_inp,clk,rst,'0',selA_div,selB_div,selC_div,RES_DIV,pt2,wt2);


  -- sel   wr  addr
  --  0    0   0000   ativa selA (SW A) MUL
  --  0    0   0001   ativa selB (SW B) MUL
  --  0    1   0000   ativa selC (LW C) MUL

  --  0    0   0100   ativa selA (SW A) SUM
  --  0    0   0101   ativa selB (SW B) SUM
  --  0    1   0100   ativa selC (LW C) SUM

  --  0    0   1100   ativa selA (SW A) DIV
  --  0    0   1101   ativa selB (SW B) DIV
  --  0    1   1100   ativa selC (LW C) DIV

  --  1    x   xxxx   *#NOP#*

  selA_mul <= '1' when sel = '0' and addr = "0000" and wr = '0' else '0';
  selB_mul <= '1' when sel = '0' and addr = "0001" and wr = '0' else '0';
  selC_mul <= '1' when sel = '0' and addr = "0000" and wr = '1' else '0';

  selA_sum <= '1' when sel = '0' and addr = "0010" and wr = '0' else '0';
  selB_sum <= '1' when sel = '0' and addr = "0011" and wr = '0' else '0';
  selC_sum <= '1' when sel = '0' and addr = "0010" and wr = '1' else '0';

  --selA_div <= '1' when sel = '0' and addr = "0100" and wr = '0' else '0';
  --selB_div <= '1' when sel = '0' and addr = "0101" and wr = '0' else '0';
  --selC_div <= '1' when sel = '0' and addr = "0100" and wr = '1' else '0';

  wt_mul <= not(selC_mul);
  wt_sum <= not(selC_sum);
  --wt_div <= not(selC_div);

  U_WAIT_ON_READS_MUL: component wait_states
    generic map (1) port map (rst, clk, wt_mul, wt_st0);
  U_WAIT_ON_READS_SUM: component wait_states
    generic map (1) port map (rst, clk, wt_sum, wt_st1);
  --U_WAIT_ON_READS_DIV: component wait_states
    --generic map (1) port map (rst, clk, wt_div, wt_st2);

  rdy <= not(wt_st0 or wt_st1 or (wt0 and selC_mul) or (wt1 and selC_sum)); --or wt_st2 or (wt2 and selC_div));

  data_out <= RES_MUL when selC_mul = '1' else
              RES_SUM when selC_sum = '1' else
              (others => 'X');
              --RES_DIV when selC_div = '1' else
end estrutural;
-- ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++


-- ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
-- fake_FPU
-- ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
library IEEE;
use IEEE.std_logic_1164.all;
use work.p_wires.all;

entity fake_FPU is
  port(rst      : in    std_logic;
       clk      : in    std_logic;
       sel      : in    std_logic;
       rdy      : out   std_logic;
       wr       : in    std_logic;
       addr     : in    std_logic_vector;
       data_inp : in    reg32;
       data_out : out   reg32);
end fake_FPU;

architecture estrutural of fake_FPU is
begin
  rdy <= '1';
  data_out <= (others => 'X');
end estrutural;
