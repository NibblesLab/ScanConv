
-- VHDL Instantiation Created from source file ckgen.vhd -- 18:28:19 03/29/2009
--
-- Notes: 
-- 1) This instantiation template has been automatically generated using types
-- std_logic and std_logic_vector for the ports of the instantiated module
-- 2) To use this template to instantiate this entity, cut-and-paste and then edit

	COMPONENT ckgen
	PORT(
		U1_CLKIN_IN : IN std_logic;
		U1_RST_IN : IN std_logic;          
		U1_CLKFX_OUT : OUT std_logic;
		U1_CLKIN_IBUFG_OUT : OUT std_logic;
		U1_CLK0_OUT : OUT std_logic;
		U1_STATUS_OUT : OUT std_logic_vector(7 downto 0);
		U2_CLKFX_OUT : OUT std_logic;
		U2_CLK0_OUT : OUT std_logic;
		U2_LOCKED_OUT : OUT std_logic;
		U2_STATUS_OUT : OUT std_logic_vector(7 downto 0)
		);
	END COMPONENT;

	Inst_ckgen: ckgen PORT MAP(
		U1_CLKIN_IN => ,
		U1_RST_IN => ,
		U1_CLKFX_OUT => ,
		U1_CLKIN_IBUFG_OUT => ,
		U1_CLK0_OUT => ,
		U1_STATUS_OUT => ,
		U2_CLKFX_OUT => ,
		U2_CLK0_OUT => ,
		U2_LOCKED_OUT => ,
		U2_STATUS_OUT => 
	);


