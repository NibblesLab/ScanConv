--
-- Scan Converter for RGBI by Spartan-3E
--
--   (c)2008-2014 Nibbles Lab.
--

-- SW
-- 1  CONV RGB
-- 2       RGBI
-- 3       Mono-8
-- 4       Mono-16
-- 5       Green-8
-- 6       Green-16
-- 7  THRU RGB
-- 8       RGBI
-- 9       Mono-8
-- 10      Mono-16
-- 11      Green-8
-- 12      Green-16

library IEEE;
use IEEE.STD_LOGIC_1164.ALL;
use IEEE.STD_LOGIC_ARITH.ALL;
use IEEE.STD_LOGIC_UNSIGNED.ALL;

---- Uncomment the following library declaration if instantiating
---- any Xilinx primitives in this code.
library UNISIM;
use UNISIM.VComponents.all;

entity ScanConv is
    Port ( CLK : in  STD_LOGIC;
           RI : in  STD_LOGIC;
           GI : in  STD_LOGIC;
           BI : in  STD_LOGIC;
           II : in  STD_LOGIC;
           VSI : in  STD_LOGIC;
           HSI : in  STD_LOGIC;
           RO : out  STD_LOGIC_VECTOR (3 downto 0);
           GO : out  STD_LOGIC_VECTOR (3 downto 0);
           BO : out  STD_LOGIC_VECTOR (3 downto 0);
           VSO : out  STD_LOGIC;
           HSO : out  STD_LOGIC;
			  LED : out std_logic;
           SW : in  STD_LOGIC_VECTOR (1 to 12));
end ScanConv;

architecture Behavioral of ScanConv is

signal RD : std_logic_vector(3 downto 0);
signal GD : std_logic_vector(3 downto 0);
signal BD : std_logic_vector(3 downto 0);
signal CTR100M : std_logic_vector(12 downto 0);
signal TS : std_logic_vector(11 downto 0);
signal OCTR : std_logic_vector(11 downto 0);
signal ICTR : std_logic_vector(11 downto 0);
signal Hi : std_logic_vector(5 downto 0);
signal Si : std_logic_vector(5 downto 0);
signal BUFO : std_logic_vector(3 downto 0);
signal BUFI : std_logic_vector(3 downto 0);
signal IGRB : std_logic_vector(3 downto 0);
signal HS : std_logic;
signal HBLANK : std_logic;
signal CLK100 : std_logic;
signal CLK64 : std_logic;

--	COMPONENT ckgen0
--	PORT(
--		CLKIN_IN : IN std_logic;
--		RST_IN : IN std_logic;          
--		CLKFX_OUT : OUT std_logic;
--		CLKIN_IBUFG_OUT : OUT std_logic;
--		CLK0_OUT : OUT std_logic
--		);
--	END COMPONENT;

--	COMPONENT ckgen1
--	PORT(
--		CLKIN_IN : IN std_logic;          
--		CLKFX_OUT : OUT std_logic;
--		CLKIN_IBUFG_OUT : OUT std_logic;
--		CLK0_OUT : OUT std_logic;
--		LOCKED_OUT : OUT std_logic
--		);
--	END COMPONENT;

--	component BUFG
--	port(
--		I : in std_logic;
--		O : out std_logic
--		);
--	end component;

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

begin

	VSO<=VSI;
	HSO<=HS when SW(7 to 12)="111111" else HSI;
	RO<="0000" when HBLANK='1' and SW(7 to 12)="111111" else RD;
	GO<="0000" when HBLANK='1' and SW(7 to 12)="111111" else GD;
	BO<="0000" when HBLANK='1' and SW(7 to 12)="111111" else BD;

	LED<='0';

	process( CLK64 ) begin
		if CLK64'event and CLK64='1' then

			-- Filtering HSI
			Si<=Si(4 downto 0)&HSI;

			-- Counter start
			if Si="111000" then
				ICTR<=X"EE0";
			else
				ICTR<=ICTR+'1';
			end if;

		end if;
	end process;

	BUFI<=II&GI&RI&BI;

	process( CLK100 ) begin
		if CLK100'event and CLK100='1' then

			-- Filtering HSI
			Hi<=Hi(4 downto 0)&HSI;

			-- Counter start
			if Hi="111000" then
				CTR100M<=(others=>'0');
				TS<=CTR100M(12 downto 1);
				OCTR<=(others=>'0');
			elsif OCTR=TS then
				OCTR<=(others=>'0');
				CTR100M<=CTR100M+'1';
			else
				CTR100M<=CTR100M+'1';
				OCTR<=OCTR+'1';
			end if;

			-- Horizontal Sync genarate
			if OCTR=0 then
				HS<='0';
			elsif OCTR=354 then	--384
				HS<='1';
			end if;

			-- Horizontal Blanking Time counter
			if OCTR=0 then
				HBLANK<='1';
			elsif OCTR=466 then	--496
				HBLANK<='0';
			elsif OCTR=3136 then	--3136
				HBLANK<='1';
			end if;

		end if;
	end process;

	IGRB<=BUFO when SW(7 to 12)="111111" else BUFI;
	process( SW, IGRB ) begin
		if SW(1)='0' or SW(7)='0' then	--	RGB
			case IGRB is
--				when "0000"|"1000" =>
--					RD<=(others=>'0');
--					BD<=(others=>'0');
--					GD<=(others=>'0');
				when "0001"|"1001" =>
					RD<=(others=>'0');
					BD<=(others=>'1');
					GD<=(others=>'0');
				when "0010"|"1010" =>
					RD<=(others=>'1');
					BD<=(others=>'0');
					GD<=(others=>'0');
				when "0011"|"1011" =>
					RD<=(others=>'1');
					BD<=(others=>'1');
					GD<=(others=>'0');
				when "0100"|"1100" =>
					RD<=(others=>'0');
					BD<=(others=>'0');
					GD<=(others=>'1');
				when "0101"|"1101" =>
					RD<=(others=>'0');
					BD<=(others=>'1');
					GD<=(others=>'1');
				when "0110"|"1110" =>
					RD<=(others=>'1');
					BD<=(others=>'0');
					GD<=(others=>'1');
				when "0111"|"1111" =>
					RD<=(others=>'1');
					BD<=(others=>'1');
					GD<=(others=>'1');
				when others =>
					RD<=(others=>'0');
					BD<=(others=>'0');
					GD<=(others=>'0');
			end case;
		elsif SW(2)='0' or SW(8)='0' then	--	RGBI
			case IGRB is
--				when "0000" =>
--					RD<=(others=>'0');
--					BD<=(others=>'0');
--					GD<=(others=>'0');
				when "0001" =>
					RD<=(others=>'0');
					BD<="1000";
					GD<=(others=>'0');
				when "0010" =>
					RD<="1000";
					BD<=(others=>'0');
					GD<=(others=>'0');
				when "0011" =>
					RD<="1000";
					BD<="1000";
					GD<=(others=>'0');
				when "0100" =>
					RD<=(others=>'0');
					BD<=(others=>'0');
					GD<="1000";
				when "0101" =>
					RD<=(others=>'0');
					BD<="1000";
					GD<="1000";
				when "0110" =>
					RD<="1000";
					BD<=(others=>'0');
					GD<="1000";
				when "0111" =>
					RD<="1000";
					BD<="1000";
					GD<="1000";
				when "1000" =>
					RD<="0111";
					BD<="0111";
					GD<="0111";
				when "1001" =>
					RD<=(others=>'0');
					BD<=(others=>'1');
					GD<=(others=>'0');
				when "1010" =>
					RD<=(others=>'1');
					BD<=(others=>'0');
					GD<=(others=>'0');
				when "1011" =>
					RD<=(others=>'1');
					BD<=(others=>'1');
					GD<=(others=>'0');
				when "1100" =>
					RD<=(others=>'0');
					BD<=(others=>'0');
					GD<=(others=>'1');
				when "1101" =>
					RD<=(others=>'0');
					BD<=(others=>'1');
					GD<=(others=>'1');
				when "1110" =>
					RD<=(others=>'1');
					BD<=(others=>'0');
					GD<=(others=>'1');
				when "1111" =>
					RD<=(others=>'1');
					BD<=(others=>'1');
					GD<=(others=>'1');
				when others =>
					RD<=(others=>'0');
					BD<=(others=>'0');
					GD<=(others=>'0');
			end case;
		elsif SW(3)='0' or SW(9)='0' then	--	Mono-8
			case IGRB is
--				when "0000" =>
--					RD<=(others=>'0');
--					BD<=(others=>'0');
--					GD<=(others=>'0');
				when "1001"|"0001" =>
					RD<="0101";
					BD<="0101";
					GD<="0101";
				when "1010"|"0010" =>
					RD<="1000";
					BD<="1000";
					GD<="1000";
				when "1011"|"0011" =>
					RD<="1001";
					BD<="1001";
					GD<="1001";
				when "1100"|"0100" =>
					RD<="1100";
					BD<="1100";
					GD<="1100";
				when "1101"|"0101" =>
					RD<="1101";
					BD<="1101";
					GD<="1101";
				when "1110"|"0110" =>
					RD<="1110";
					BD<="1110";
					GD<="1110";
				when "1111"|"0111" =>
					RD<=(others=>'1');
					BD<=(others=>'1');
					GD<=(others=>'1');
				when others =>
					RD<=(others=>'0');
					BD<=(others=>'0');
					GD<=(others=>'0');
			end case;
		elsif SW(4)='0' or SW(10)='0' then	--	Mono-16
			case IGRB is
--				when "0000" =>
--					RD<=(others=>'0');
--					BD<=(others=>'0');
--					GD<=(others=>'0');
				when "0001" =>
					RD<="0001";
					BD<="0001";
					GD<="0001";
				when "0010" =>
					RD<="0010";
					BD<="0010";
					GD<="0010";
				when "0011" =>
					RD<="0011";
					BD<="0011";
					GD<="0011";
				when "0100" =>
					RD<="0110";
					BD<="0110";
					GD<="0110";
				when "0101" =>
					RD<="0111";
					BD<="0111";
					GD<="0111";
				when "0110" =>
					RD<="1010";
					BD<="1010";
					GD<="1010";
				when "0111" =>
					RD<="1011";
					BD<="1011";
					GD<="1011";
				when "1000" =>
					RD<="0100";
					BD<="0100";
					GD<="0100";
				when "1001" =>
					RD<="0101";
					BD<="0101";
					GD<="0101";
				when "1010" =>
					RD<="1000";
					BD<="1000";
					GD<="1000";
				when "1011" =>
					RD<="1001";
					BD<="1001";
					GD<="1001";
				when "1100" =>
					RD<="1100";
					BD<="1100";
					GD<="1100";
				when "1101" =>
					RD<="1101";
					BD<="1101";
					GD<="1101";
				when "1110" =>
					RD<="1110";
					BD<="1110";
					GD<="1110";
				when "1111" =>
					RD<=(others=>'1');
					BD<=(others=>'1');
					GD<=(others=>'1');
				when others =>
					RD<=(others=>'0');
					BD<=(others=>'0');
					GD<=(others=>'0');
			end case;
		elsif SW(5)='0' or SW(11)='0' then	--	Green-8
			case IGRB is
				when "0000"|"1000" =>
					RD<=(others=>'0');
					BD<=(others=>'0');
					GD<=(others=>'0');
				when others =>
					RD<=(others=>'0');
					BD<=(others=>'0');
					GD<=(others=>'1');
			end case;
		elsif SW(6)='0' or SW(12)='0' then	--	Green-16
			case IGRB is
				when "0000" =>
					RD<=(others=>'0');
					BD<=(others=>'0');
					GD<=(others=>'0');
				when others =>
					RD<=(others=>'0');
					BD<=(others=>'0');
					GD<=(others=>'1');
			end case;
		else
			RD<=(others=>'0');
			BD<=(others=>'0');
			GD<=(others=>'0');
		end if;
	end process;

   BUF0 : RAMB16_S4_S4
   generic map (
      INIT_A => X"0", --  Value of output RAM registers on Port A at startup
      INIT_B => X"0", --  Value of output RAM registers on Port B at startup
      SRVAL_A => X"0", --  Port A ouput value upon SSR assertion
      SRVAL_B => X"0", --  Port B ouput value upon SSR assertion
      WRITE_MODE_A => "WRITE_FIRST", --  WRITE_FIRST, READ_FIRST or NO_CHANGE
      WRITE_MODE_B => "WRITE_FIRST", --  WRITE_FIRST, READ_FIRST or NO_CHANGE
      SIM_COLLISION_CHECK => "ALL", -- "NONE", "WARNING", "GENERATE_X_ONLY", "ALL" 
      -- The following INIT_xx declarations specify the initial contents of the RAM
      -- Address 0 to 1023
      INIT_00 => X"0000000000000000000000000000000000000000000000000000000000000000",
      INIT_01 => X"0000000000000000000000000000000000000000000000000000000000000000",
      INIT_02 => X"0000000000000000000000000000000000000000000000000000000000000000",
      INIT_03 => X"0000000000000000000000000000000000000000000000000000000000000000",
      INIT_04 => X"0000000000000000000000000000000000000000000000000000000000000000",
      INIT_05 => X"0000000000000000000000000000000000000000000000000000000000000000",
      INIT_06 => X"0000000000000000000000000000000000000000000000000000000000000000",
      INIT_07 => X"0000000000000000000000000000000000000000000000000000000000000000",
      INIT_08 => X"0000000000000000000000000000000000000000000000000000000000000000",
      INIT_09 => X"8888888888888888888888888888888888888888888888888888888888888888",
      INIT_0A => X"9999999999999999999999999999999999999999999999999999999999999999",
      INIT_0B => X"AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA",
      INIT_0C => X"BBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBB",
      INIT_0D => X"CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC",
      INIT_0E => X"DDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDD",
      INIT_0F => X"EEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEE",
      -- Address 1024 to 2047
      INIT_10 => X"FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF",
      INIT_11 => X"0000000000000000000000000000000000000000000000000000000000000000",
      INIT_12 => X"1111111111111111111111111111111111111111111111111111111111111111",
      INIT_13 => X"2222222222222222222222222222222222222222222222222222222222222222",
      INIT_14 => X"3333333333333333333333333333333333333333333333333333333333333333",
      INIT_15 => X"4444444444444444444444444444444444444444444444444444444444444444",
      INIT_16 => X"5555555555555555555555555555555555555555555555555555555555555555",
      INIT_17 => X"6666666666666666666666666666666666666666666666666666666666666666",
      INIT_18 => X"7777777777777777777777777777777777777777777777777777777777777777",
      INIT_19 => X"8888888888888888888888888888888888888888888888888888888888888888",
      INIT_1A => X"9999999999999999999999999999999999999999999999999999999999999999",
      INIT_1B => X"AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA",
      INIT_1C => X"BBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBB",
      INIT_1D => X"CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC",
      INIT_1E => X"DDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDD",
      INIT_1F => X"EEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEE",
      -- Address 2048 to 3071
      INIT_20 => X"FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF",
      INIT_21 => X"0000000000000000000000000000000000000000000000000000000000000000",
      INIT_22 => X"1111111111111111111111111111111111111111111111111111111111111111",
      INIT_23 => X"2222222222222222222222222222222222222222222222222222222222222222",
      INIT_24 => X"3333333333333333333333333333333333333333333333333333333333333333",
      INIT_25 => X"4444444444444444444444444444444444444444444444444444444444444444",
      INIT_26 => X"5555555555555555555555555555555555555555555555555555555555555555",
      INIT_27 => X"6666666666666666666666666666666666666666666666666666666666666666",
      INIT_28 => X"7777777777777777777777777777777777777777777777777777777777777777",
      INIT_29 => X"8888888888888888888888888888888888888888888888888888888888888888",
      INIT_2A => X"9999999999999999999999999999999999999999999999999999999999999999",
      INIT_2B => X"AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA",
      INIT_2C => X"BBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBB",
      INIT_2D => X"CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC",
      INIT_2E => X"DDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDD",
      INIT_2F => X"EEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEE",
      -- Address 3072 to 4095
      INIT_30 => X"FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF",
      INIT_31 => X"0000000000000000000000000000000000000000000000000000000000000000",
      INIT_32 => X"0000000000000000000000000000000000000000000000000000000000000000",
      INIT_33 => X"0000000000000000000000000000000000000000000000000000000000000000",
      INIT_34 => X"0000000000000000000000000000000000000000000000000000000000000000",
      INIT_35 => X"0000000000000000000000000000000000000000000000000000000000000000",
      INIT_36 => X"0000000000000000000000000000000000000000000000000000000000000000",
      INIT_37 => X"0000000000000000000000000000000000000000000000000000000000000000",
      INIT_38 => X"0000000000000000000000000000000000000000000000000000000000000000",
      INIT_39 => X"0000000000000000000000000000000000000000000000000000000000000000",
      INIT_3A => X"0000000000000000000000000000000000000000000000000000000000000000",
      INIT_3B => X"0000000000000000000000000000000000000000000000000000000000000000",
      INIT_3C => X"0000000000000000000000000000000000000000000000000000000000000000",
      INIT_3D => X"0000000000000000000000000000000000000000000000000000000000000000",
      INIT_3E => X"0000000000000000000000000000000000000000000000000000000000000000",
      INIT_3F => X"0000000000000000000000000000000000000000000000000000000000000000")
   port map (
      DOA => open,     -- Port A 4-bit Data Output
      DOB => BUFO,     -- Port B 4-bit Data Output
      ADDRA => ICTR,   -- Port A 12-bit Address Input
      ADDRB => OCTR,   -- Port B 12-bit Address Input
      CLKA => CLK64,   -- Port A Clock
      CLKB => CLK100,  -- Port B Clock
      DIA => BUFI,     -- Port A 4-bit Data Input
      DIB => "0000",   -- Port B 4-bit Data Input
      ENA => '1',      -- Port A RAM Enable Input
      ENB => '1',      -- Port B RAM Enable Input
      SSRA => '0',     -- Port A Synchronous Set/Reset Input
      SSRB => '0',     -- Port B Synchronous Set/Reset Input
      WEA => '1',      -- Port A Write Enable Input
      WEB => '0'       -- Port B Write Enable Input
   );

--	CKG0: ckgen0 PORT MAP(
--		CLKIN_IN => CLK32,
--		RST_IN => not LOCKDCM1,
--		CLKFX_OUT => CLK100,
--		CLKIN_IBUFG_OUT => open,
--		CLK0_OUT => open
--	);

--	BUFG0 : BUFG port map(
--		I=>CLK_dup,
--		O=>CLK32
--	);

--	CKG1: ckgen1 PORT MAP(
--		CLKIN_IN => CLK,
--		CLKFX_OUT => CLK64,	-- 56.9MHz
--		CLKIN_IBUFG_OUT => open,
--		CLK0_OUT => CLK32,
--		LOCKED_OUT => LOCKDCM1
--	);

	Inst_ckgen: ckgen PORT MAP(
		U1_CLKIN_IN => CLK,
		U1_RST_IN => '0',
		U1_CLKFX_OUT => CLK64,
		U1_CLKIN_IBUFG_OUT => open,
		U1_CLK0_OUT => open,
		U1_STATUS_OUT => open,
		U2_CLKFX_OUT => CLK100,
		U2_CLK0_OUT => open,
		U2_LOCKED_OUT => open,
		U2_STATUS_OUT => open
	);

end Behavioral;

