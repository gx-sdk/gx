// gx language implementation
// Copyright (C) 2015 Alex Iadicicco <http://ajitek.net>
//
// For licensing information, refer to the COPYING file
// in the project root

//! A compiler's most important user-facing feature, aside from the compilation
//! itself, is arguably its error handling mechanism.
//!
//! At the center is the `Message` struct, which contains a `kind` field to
//! indicate the kind of message, a `msg` containing the actual text of the
//! message, and optional `start` and `end` fields indicating the location or
//! range of locations that the message concerns.
//!
//! A `MessageList` struct is a wrapper around a vector of `Message`s. It's
//! often more useful to return a `Result<T, MessageList>` than a `Result<T,
//! Message>`. A simple example is converting a parse tree into a semantic
//! graph. Encountering an error in one function should not halt compilation
//! altogether, since errors in later functions may be useful to the user.
//! Therefore, it would make sense for the error case in semantic conversion to
//! return a `MessageList` rather than a single `Message`

use std::fmt;

/// Represents a position in the input. Both `line` and `col` can take a
/// value of 0 to indicate that neither is relevant to the condition at
/// hand. If `line` is 0, then `col` can reasonably be assumed to be 0.
pub struct Position {
    pub file:          String,
    pub line:          usize,
    pub col:           usize,
}

impl fmt::Debug for Position {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        if self.line == 0 {
            f.write_str(&self.file[..])
        } else if self.col == 0 {
            write!(f, "{}:{}", self.file, self.line)
        } else {
            write!(f, "{}:{}:{}", self.file, self.line, self.col)
        }
    }
}

/// A structure representing a generic error to be reported. If `end` is `None`,
/// then the error occurs exactly at `start`. If `start` is `None`, then the
/// error concerns the entire environment.
pub struct Message {
    pub kind:          MessageKind,
    pub msg:           String,
    pub start:         Option<Position>,
    pub end:           Option<Position>,
}

impl Message {
    pub fn to_list(self) -> MessageList {
        MessageList {
            msgs: vec![self]
        }
    }
}

impl fmt::Display for Message {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        // print the message kind
        try!(write!(f, "{}:", match self.kind {
            MessageKind::Internal => "internal",
            MessageKind::Fatal    => "fatal",
            MessageKind::Error    => "error",
            MessageKind::Warning  => "warning",
            MessageKind::Info     => "note"
        }));

        // then the range
        match self.start {
            None => {}
            Some(ref start) => {
                try!(write!(f, " {:?}", start));
                match self.end {
                    None => {}
                    Some(ref end) => {
                        let put_file =              start.file != end.file;
                        let put_line = put_file || (start.line != end.line);
                        let put_col  = put_line || (start.col  != end.col);
                        let mut colon = "";

                        try!(write!(f, "-"));
                        if put_file {
                            try!(write!(f, "{}", end.file));
                            colon = ":";
                        }
                        if put_line && end.line != 0 {
                            try!(write!(f, "{}{}", colon, end.line));
                            colon = ":";
                        }
                        if put_col && end.col != 0 {
                            try!(write!(f, "{}{}", colon, end.col));
                        }
                    }
                }
                try!(write!(f, ":"));
            }
        }

        // then the actual text of the message
        try!(write!(f, " {}", self.msg));

        Ok(())
    }
}

/// Used to indicate the severity or context of an error message.
pub enum MessageKind {
    /// The error originated from within the compiler itself. The occurrence
    /// of such an error is a bug and should be reported.
    Internal,

    /// The error is unrecoverable; compiling stops immediately. This should
    /// be used for cases not directly concerning the input, such as errors
    /// in the environment or with the command line.
    Fatal,

    /// The error is unrecoverable; compiling stops immediately. This should
    /// be used only for cases concerning the input, such as syntax errors
    /// or semantic errors.
    Error,

    /// The error is recoverable but the user should still be notified of
    /// the condition. Should be used to indicate potential problems with the
    /// input.
    Warning,

    /// The message is a diagnostic message. Potential use cases include
    /// informing the user of missed optimization opportunities.
    Info
}

/// A list of messages.
pub struct MessageList {
    pub msgs:  Vec<Message>
}

impl fmt::Display for MessageList {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        for m in self.msgs.iter() {
            try!(write!(f, "{}\n", m));
        }
        Ok(())
    }
}
