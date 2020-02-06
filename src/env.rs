use std::{collections::HashMap, fmt::Debug, hash::Hash};

#[derive(Debug)]
struct Frame<K: Hash + PartialEq + Eq, V: Hash + PartialEq + Eq> {
    map: HashMap<K, V>,
    watching: Vec<K>,
}

impl<K, V> Default for Frame<K, V>
where
    K: Hash + PartialEq + Eq,
    V: Hash + PartialEq + Eq,
{
    fn default() -> Self {
        Self {
            map: Default::default(),
            watching: Default::default(),
        }
    }
}

#[derive(Debug)]
pub struct Env<K: Hash + PartialEq + Eq, V: Hash + PartialEq + Eq> {
    frames: Vec<Frame<K, V>>,
}

impl<K, V> Default for Env<K, V>
where
    K: Hash + PartialEq + Eq,
    V: Hash + PartialEq + Eq,
{
    fn default() -> Self {
        Self {
            frames: Default::default(),
        }
    }
}

impl<K, V> Env<K, V>
where
    K: Hash + PartialEq + Eq + Debug,
    V: Hash + PartialEq + Eq + Debug,
{
    /// Returns a new Env with one frame initialized.
    pub fn new() -> Self {
        Self {
            frames: vec![Default::default()],
        }
    }

    pub fn push_frame_and_watch(&mut self, watching: Vec<K>) {
        self.frames.push(Frame {
            map: Default::default(),
            watching,
        })
    }

    pub fn pop_frame(&mut self) {
        if let Some(Frame { mut map, watching }) = self.frames.pop() {
            if let Some(parent_frame) = self.frames.last_mut() {
                for key in watching {
                    if let Some(value) = map.remove(&key) {
                        parent_frame.map.insert(key, value);
                    }
                }
            }
        }
    }

    pub fn insert_local(&mut self, key: K, value: V) {
        self.frames
            .last_mut()
            .expect("at least one frame exists")
            .map
            .insert(key, value);
    }

    pub fn get(&self, key: &K) -> Option<&V> {
        for frame in self.frames.iter().rev() {
            if let Some(value) = frame.map.get(key) {
                return Some(value);
            }
        }
        None
    }

    pub fn contains_key(&self, key: &K) -> bool {
        for frame in self.frames.iter().rev() {
            if frame.map.contains_key(key) {
                return true;
            }
        }
        false
    }

    pub fn remove(&mut self, key: &K) -> Option<V> {
        for frame in self.frames.iter_mut().rev() {
            if let Some(value) = frame.map.remove(key) {
                return Some(value);
            }
        }
        None
    }
}

#[test]
fn extend() {
    let mut env = Env::new();
    env.insert_local("global_1", vec!["G1"]);
    env.push_frame_and_watch(vec![]);
    {
        env.insert_local("local_1", vec!["L1"]);
        assert_eq!(env.get(&"global_1"), Some(&vec!["G1"]));
        assert_eq!(env.get(&"local_1"), None);
    }
    env.pop_frame();
    assert_eq!(env.get(&"global_1"), Some(&vec!["G1"]));
    assert_eq!(env.get(&"local_1"), None);
}
