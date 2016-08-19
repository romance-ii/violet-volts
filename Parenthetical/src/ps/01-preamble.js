/* -*- javascript -*- */

/*
 * This preamble will be inserted into the combined JavaScript source
 * file, which otherwise consists entirely of code produced
 * via Parenscript.
 */


/* The @preserve/@license tags leader let minifiers know to preserve
 * the notices */

/** @license
 * Romance II Game Client Library
 *
 * Copyright © 2009-2013, Bruce-Robert Pocock.
 *
* This program is free software: you can redistribute it and/or modify
* it under the terms of the GNU Affero General Public License as
* published by the Free Software Foundation, either version 3 of the
* License, or (at your option) any later version.
*
* This program is distributed in the hope that it will be useful, but
* WITHOUT ANY WARRANTY; without even the implied warranty of
* MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
* Affero General Public License for more details.
* 
* You should have received a copy of the GNU Affero General Public
* License along with this program.  If not, see
* http://www.gnu.org/licenses/
 */

if (!String.prototype.format) {
  String.prototype.format = function() {
    var args = arguments;
    return this.replace
      (/{[\d+]}/g,
       function(match, number)
       { return (typeof args[number] != 'undefined'
                 ? args[number]
                 : match);});
}}

