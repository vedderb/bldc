/*
    Copyright 2025 Joel Svensson  svenssonjoel@yahoo.se

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
*/

#ifndef LBM_MCP_H_
#define LBM_MCP_H_

#include <stdbool.h>
#include <stdint.h>

/* Override the documentation directory (default: "../doc").
 * Must be called before lbm_mcp_run if a non-default path is needed.
 */
void lbm_mcp_set_doc_path(const char *path);

/* Register a callback that performs a clean LispBM restart.
 * The callback must reinitialise LispBM and leave the evaluator paused.
 * lbm_mcp_run re-registers its own callbacks and resumes the evaluator.
 */
void lbm_mcp_set_reset_callback(bool (*cb)(void));

/* Register a callback that restarts LispBM with new resource sizes.
 * heap_cells: new heap size in cons cells (0 = keep current).
 * memory_bytes: new flat memory size in bytes (0 = keep current).
 * Same post-conditions as the reset callback.
 */
void lbm_mcp_set_reinit_callback(bool (*cb)(uint32_t heap_cells, uint32_t memory_bytes));

/* Start the MCP (Model Context Protocol) stdio server.
 * Reads JSON-RPC 2.0 messages from stdin and writes responses to stdout.
 * Blocks until stdin is closed.
 * Assumes lbm_init and lbm_run_eval have already been called.
 */
void lbm_mcp_run(void);

#endif
