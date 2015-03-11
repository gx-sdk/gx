// gx language implementation
// Copyright (C) 2015 Alex Iadicicco <http://ajitek.net>
//
// For licensing information, refer to the COPYING file
// in the project root

//! Semantic information for units

use std::collections::HashMap;
use std::fmt;

use msg;
use semantic::*;

pub struct Unit<'a> {
    pub units:     HashMap<String, Unit<'a>>,
    pub types:     HashMap<String, types::Type<'a>>,
}
