define connect
	target extended localhost:3333
end

define dump_flash
	if ($argc == 0)
		dump binary memory flash_dump.bin 0 0x100000
	end
	if ($argc == 1)
		dump binary memory $arg0 0 0x100000
	else
		help dump_flash
	end
end
document dump_flash
Dump the enitire stm flash area to a bin file.
Usage: dump_flash <optional> *.bin
end
