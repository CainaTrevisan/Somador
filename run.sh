ghdl -a --ieee=synopsys -fexplicit components.vhd somador.vhd
ghdl -a --ieee=synopsys -fexplicit somador_tb.vhd
ghdl -e --ieee=synopsys -fexplicit somador_tb
./somador_tb --stop-time=80us --vcd=somador.vcd

