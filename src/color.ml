type 'a t = {
  t: 'a;
  channels: int;
  has_alpha: bool;
}

let create  ~has_alpha ~channels t = {t; channels; has_alpha}
let has_alpha {has_alpha;_} = has_alpha
let channels {channels;_} = channels
let t {t;_} = t
