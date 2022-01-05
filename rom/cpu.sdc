create_clock -name {CLOCK_50} -period 20.000 -waveform {0.000 10.000} [get_ports {CLOCK_50}]
derive_pll_clocks
derive_clock_uncertainty
set_false_path -from * -to [get_ports {LED[*]}]
set_false_path -from [get_ports {RST}] -to *
