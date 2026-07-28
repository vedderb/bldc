
# Third-Party Notices

This file lists third-party code that has been incorporated into LispBM

## LibSchrift

   https://www.github.com/tomolt/libschrift

   ISC License

   © 2019-2022 Thomas Oltmann and contributors

   Permission to use, copy, modify, and/or distribute this software for any
   purpose with or without fee is hereby granted, provided that the above
   copyright notice and this permission notice appear in all copies.

   THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES
   WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF
   MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR
   ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES
   WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN
   ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF
   OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.


## TJpgDec

   https://elm-chan.org/fsw/tjpgd/

   TJpgDec License

   © ChaN, 2021

   The TJpgDec is a generic JPEG decompressor module for tiny embedded systems.
   This is a free software that opened for education, research and commercial
   developments under license policy of following terms.

   Copyright (C) 2021, ChaN, all rights reserved.

    * The TJpgDec module is a free software and there is NO WARRANTY.
    * No restriction on use. You can use, modify and redistribute it for
      personal, non-profit or commercial products UNDER YOUR RESPONSIBILITY.
    * Redistributions of source code must retain the above copyright notice.


## Codemirror Scheme editor

   In repl-wasm a scheme code editor from codemirror.net is used.
   This code is stored into repl-wasm/scripts/codemirror and is released
   under the following license:

   Version: 5.65.16

   MIT License

   Copyright (C) 2017 by Marijn Haverbeke <marijnh@gmail.com> and others

   Permission is hereby granted, free of charge, to any person obtaining a copy
   of this software and associated documentation files (the "Software"), to deal
   in the Software without restriction, including without limitation the rights
   to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
   copies of the Software, and to permit persons to whom the Software is
   furnished to do so, subject to the following conditions:

   The above copyright notice and this permission notice shall be included in
   all copies or substantial portions of the Software.

   THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
   IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
   FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
   AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
   LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
   OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN
   THE SOFTWARE.

## uPlot

   In repl-wasm uPlot is used for plotting of graphs. The code
   is stored in repl-wasm/scripts/uplot and is released under the following
   license:

   Version: 1.6.31

   MIT License

   Copyright (c) 2021 Leon Sorokin

   Permission is hereby granted, free of charge, to any person obtaining a copy
   of this software and associated documentation files (the "Software"), to deal
   in the Software without restriction, including without limitation the rights
   to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
   copies of the Software, and to permit persons to whom the Software is
   furnished to do so, subject to the following conditions:

   The above copyright notice and this permission notice shall be included in
   all copies or substantial portions of the Software.

   THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
   IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
   FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
   AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
   LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
   OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN
   THE SOFTWARE.

## QCustomPlot

The LispBM Qt5 utilities uses the QCustomPlot library for graph rendering.
The code is stored under /utils/qt and is released under the following
license:

/***************************************************************************
**                                                                        **
**  QCustomPlot, an easy to use, modern plotting widget for Qt            **
**  Copyright (C) 2011-2022 Emanuel Eichhammer                            **
**                                                                        **
**  This program is free software: you can redistribute it and/or modify  **
**  it under the terms of the GNU General Public License as published by  **
**  the Free Software Foundation, either version 3 of the License, or     **
**  (at your option) any later version.                                   **
**                                                                        **
**  This program is distributed in the hope that it will be useful,       **
**  but WITHOUT ANY WARRANTY; without even the implied warranty of        **
**  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the         **
**  GNU General Public License for more details.                          **
**                                                                        **
**  You should have received a copy of the GNU General Public License     **
**  along with this program.  If not, see http://www.gnu.org/licenses/.   **
**                                                                        **
****************************************************************************
**           Author: Emanuel Eichhammer                                   **
**  Website/Contact: https://www.qcustomplot.com/                         **
**             Date: 06.11.22                                             **
**          Version: 2.1.1                                                **
****************************************************************************/

## cJSON

   The LispBM MCP server uses cJSON for JSON parsing and serialisation.
   The code is stored under `utils/mcp/` and is released under the following
   license:

   https://github.com/DaveGamble/cJSON

   MIT License

   Copyright (c) 2009-2017 Dave Gamble and cJSON contributors

   Permission is hereby granted, free of charge, to any person obtaining a copy
   of this software and associated documentation files (the "Software"), to deal
   in the Software without restriction, including without limitation the rights
   to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
   copies of the Software, and to permit persons to whom the Software is
   furnished to do so, subject to the following conditions:

   The above copyright notice and this permission notice shall be included in
   all copies or substantial portions of the Software.

   THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
   IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
   FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
   AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
   LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
   OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN
   THE SOFTWARE.


## VESC firmware

   LispBM includes several files originating in the VESC firmware projects
   [bldc](https://github.com/vedderb/bldc) and [vesc_express](https://github.com/vedderb/vesc_express).

   The following files are copyright Benjamin Vedder and licensed under the
   GNU General Public License version 3 or later:

   | File | Copyright |
   |------|-----------|
   | utils/crc.h, utils/crc.c | Copyright 2016 Benjamin Vedder |
   | utils/buffer.h, utils/buffer.c | Copyright 2016 Benjamin Vedder |
   | utils/packet.h, utils/packet.c | Copyright 2016-2021 Benjamin Vedder |

   The following files originated in VESC firmware and are co-authored; all are
   licensed under the GNU General Public License version 3 or later:

   | File | Copyright |
   |------|-----------|
   | examples/esp32c3-st7789/main/hwspi.h | Copyright 2023 Benjamin Vedder, Joel Svensson |
   | examples/esp32c3-st7789/main/hwspi.c | Copyright 2023 Benjamin Vedder, Joel Svensson |
   | examples/esp32c3-st7789/main/disp_st7789.h | Copyright 2023 Benjamin Vedder, Joel Svensson |
   | examples/esp32c3-st7789/main/disp_st7789.c | Copyright 2023 Benjamin Vedder, Joel Svensson |
   | include/extensions/display_extensions.h | Copyright 2023-2024 Benjamin Vedder, Joel Svensson |
   | src/extensions/display_extensions.c | Copyright 2023-2025 Benjamin Vedder, Joel Svensson, Rasmus Söderhielm, Joakim Lundborg (originally part of vesc_express) |
   | include/extensions/array_extensions.h | Copyright 2022 Joel Svensson, Benjamin Vedder |
   | src/extensions/array_extensions.c | Copyright 2022-2025 Joel Svensson, Benjamin Vedder |
   | include/extensions/string_extensions.h | Copyright 2022 Joel Svensson, Benjamin Vedder |
   | src/extensions/string_extensions.c | Copyright 2022-2025 Joel Svensson, Benjamin Vedder, Rasmus Söderhielm |

   GNU General Public License, Version 3

   Copyright 2016-2026 Benjamin Vedder  benjamin@vedder.se

   This program is free software: you can redistribute it and/or modify
   it under the terms of the GNU General Public License as published by
   the Free Software Foundation, either version 3 of the License, or
   (at your option) any later version.

   This program is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
   GNU General Public License for more details.

   You should have received a copy of the GNU General Public License
   along with this program.  If not, see <http://www.gnu.org/licenses/>.

