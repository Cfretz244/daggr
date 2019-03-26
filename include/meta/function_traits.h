#ifndef DAGGR_FUNCTION_TRAITS_H
#define DAGGR_FUNCTION_TRAITS_H

/*----- System Includes -----*/

#include <tuple>
#include <functional>
#include <type_traits>

/*----- Type Declarations -----*/

namespace daggr::meta {

  // Struct represents regular void.
  struct none {};
  constexpr none none_v {};

  // Detection idiom implementation.
  namespace detail {
    struct nonesuch {
      nonesuch() = delete;
      nonesuch(nonesuch const&) = delete;
      nonesuch(nonesuch&&) = delete;
      ~nonesuch() = delete;
    };
    template <class Default, class AlwaysVoid, template <class...> class Op, class... Args>
    struct detector {
      using value_t = std::false_type;
      using type = Default;
    };
    template <class Default, template <class...> class Op, class... Args>
    struct detector<Default, std::void_t<Op<Args...>>, Op, Args...> {
      using value_t = std::true_type;
      using type = Op<Args...>;
    };
  }

  // Detection idiom.
  template <template <class...> class Op, class... Args>
  using is_detected = typename detail::detector<detail::nonesuch, void, Op, Args...>::value_t;
  template <template <class...> class Op, class... Args>
  constexpr auto is_detected_v = is_detected<Op, Args...>::value;
  template <template <class...> class Op, class... Args>
  using detected_t = typename detail::detector<detail::nonesuch, void, Op, Args...>::type;
  template <class Default, template <class...> class Op, class... Args>
  using detected_or = detail::detector<Default, void, Op, Args...>;
  template <class Default, template <class...> class Op, class... Args>
  using detected_or_t = typename detail::detector<Default, void, Op, Args...>::type;

  // Calculates if a given type is a specialization of a given template.
  template <class T, template <class...> class Template>
  struct is_specialization : std::false_type {};
  template <template <class...> class Specialization, class... Params>
  struct is_specialization<Specialization<Params...>, Specialization> : std::true_type {};
  template <class T, template <class...> class Template>
  constexpr auto is_specialization_v = is_specialization<T, Template>::value;

  // Calculates if T is a specialization of std::tuple.
  template <class T>
  struct is_tuple : is_specialization<T, std::tuple> {};
  template <class T>
  constexpr auto is_tuple_v = is_tuple<T>::value;

  // Implementation of type list filtering.
  namespace detail {
    template <template <class> class Predicate, class TypeList, size_t idx, size_t... idxs>
    struct filter_type_sequence_impl {
      static constexpr size_t current_idx = idx - 1;
      using next_filter_t = std::conditional_t<
        Predicate<
          std::tuple_element_t<current_idx, TypeList>
        >::value,
        filter_type_sequence_impl<Predicate, TypeList, current_idx, current_idx, idxs...>,
        filter_type_sequence_impl<Predicate, TypeList, current_idx, idxs...>
      >;

      using type = typename next_filter_t::type;
      using index_type = typename next_filter_t::index_type;
    };
    template <template <class> class Predicate, class TypeList, size_t... idxs>
    struct filter_type_sequence_impl<Predicate, TypeList, 0, idxs...> {
      static constexpr size_t current_idx = 0;

      using type = std::tuple<
        std::tuple_element_t<idxs, TypeList>...
      >;
      using index_type = std::index_sequence<idxs...>;
    };
  }

  // Type list filtering according to some predicate.
  template <template <class> class Predicate, class... Ts>
  struct filter_type_sequence {
    using impl_type_t = detail::filter_type_sequence_impl<Predicate, std::tuple<Ts...>, sizeof...(Ts)>;

    using type = typename impl_type_t::type;
    using index_type = typename impl_type_t::index_type;
  };
  template <template <class> class Predicate, class... Ts>
  using filter_type_sequence_t = typename filter_type_sequence<Predicate, Ts...>::type;
  template <template <class> class Predicate, class... Ts>
  using filter_type_sequence_idxs_t = typename filter_type_sequence<Predicate, Ts...>::index_type;

  // Removes none from a type list to allow for regular void operations.
  template <class... Ts>
  struct filter_none {
    template <class Type>
    struct none_filter :
      std::negation<
        std::disjunction<
          std::is_same<
            std::decay_t<Type>,
            none
          >,
          std::is_same<
            std::decay_t<Type>,
            void
          >
        >
      >
    {};
    using type = typename filter_type_sequence<none_filter, Ts...>::type;
    using index_type = typename filter_type_sequence<none_filter, Ts...>::index_type;
  };
  template <class... Ts>
  using filter_none_t = typename filter_none<Ts...>::type;
  template <class... Ts>
  using filter_none_idxs_t = typename filter_none<Ts...>::index_type;

  // is_invocable implementation.
  namespace detail {
    template <class Func, class... Args>
    std::is_invocable<Func, Args...> is_invocable_impl(Func&&, std::tuple<Args...>);
    template <class Func, class... Args>
    std::is_nothrow_invocable<Func, Args...> is_nothrow_invocable_impl(Func&&, std::tuple<Args...>);
    template <class Ret, class Func, class... Args>
    std::is_invocable_r<Ret, Func, Args...> is_invocable_r_impl(Func&&, std::tuple<Args...>);
    template <class Ret, class Func, class... Args>
    std::is_nothrow_invocable_r<Ret, Func, Args...> is_nothrow_invocable_r_impl(Func&&, std::tuple<Args...>);
    template <class Func, class... Args>
    using is_invocable_t = decltype(
      is_invocable_impl(
        std::declval<Func>(),
        std::declval<filter_none_t<Args...>>()
      )
    );
    template <class Func, class... Args>
    using is_nothrow_invocable_t = decltype(
      is_nothrow_invocable_impl(
        std::declval<Func>(),
        std::declval<filter_none_t<Args...>>()
      )
    );
    template <class Func, class... Args>
    using is_invocable_r_t = decltype(
      is_invocable_r_impl(
        std::declval<Func>(),
        std::declval<filter_none_t<Args...>>()
      )
    );
    template <class Func, class... Args>
    using is_nothrow_invocable_r_t = decltype(
      is_nothrow_invocable_r_impl(
        std::declval<Func>(),
        std::declval<filter_none_t<Args...>>()
      )
    );
  }

  // Calculates whether a given function object is callable with the sequence of
  // provided arguments, filtering out any instances of none.
  template <class Func, class... Args>
  struct is_invocable : detail::is_invocable_t<Func, Args...> {
    using canonical_type = std::bool_constant<detail::is_invocable_t<Func, Args...>::value>;
  };
  template <class Func, class... Args>
  constexpr auto is_invocable_v = is_invocable<Func, Args...>::value;

  // Calculates whether a given function object could throw an exception when called with
  // the sequence of provided arguments, filtering out any instances of none.
  // Returns false if the function isn't invocable as specified.
  template <class Func, class... Args>
  struct is_nothrow_invocable : detail::is_nothrow_invocable_t<Func, Args...> {
    using canonical_type = std::bool_constant<detail::is_nothrow_invocable_t<Func, Args...>::value>;
  };
  template <class Func, class... Args>
  constexpr auto is_nothrow_invocable_v = is_nothrow_invocable<Func, Args...>::value;

  // Calculates whether a given function object returns a type implicitly convertible to
  // the given return type when called with the sequence of provided arguments, filtering
  // out any instances of none.
  // Returns false if the function isn't invocable as specified.
  template <class Ret, class Func, class... Args>
  struct is_invocable_r : detail::is_invocable_r_t<Ret, Func, Args...> {
    using canonical_type = std::bool_constant<detail::is_invocable_r_t<Ret, Func, Args...>::value>;
  };
  template <class Ret, class Func, class... Args>
  constexpr auto is_invocable_r_v = is_invocable_r<Ret, Func, Args...>::value;

  // Calculates whether a given function object could throw an exception when called with
  // the sequence of provided arguments, and if the return of said function is implicitly
  // convertible to the given return type, filtering out any instances of none.
  // Returns false if the function isn't invocable as specified.
  template <class Ret, class Func, class... Args>
  struct is_nothrow_invocable_r : detail::is_nothrow_invocable_r_t<Ret, Func, Args...> {
    using canonical_type = std::bool_constant<detail::is_nothrow_invocable_r_t<Ret, Func, Args...>::value>;
  };
  template <class Ret, class Func, class... Args>
  constexpr auto is_nothrow_invocable_r_v = is_nothrow_invocable_r<Ret, Func, Args...>::value;

  namespace detail {
    template <template <class...> class Predicate, class Func, class... Args>
    using is_unwrappable_t = std::disjunction<
      Predicate<Func, filter_none_t<Args...>>,
      Predicate<Func, Args...>
    >;
    template <template <class...> class Predicate, class Ret, class Func, class... Args>
    using is_unwrappable_r_t = std::disjunction<
      Predicate<Ret, Func, filter_none_t<Args...>>,
      Predicate<Ret, Func, Args...>
    >;
  }

  // Calculates whether a given function object is callable with the given argument,
  // or unwrapped sequence of arguments if the argument is a tuple, filtering out any
  // instances of none.
  template <class Func, class Arg>
  struct is_applicable : is_invocable<Func, Arg> {
    using typename is_invocable<Func, Arg>::canonical_type;
  };
  template <class Func, class... Args>
  struct is_applicable<Func, std::tuple<Args...>> :
    detail::is_unwrappable_t<is_invocable, Func, Args...>
  {
    using canonical_type =
      std::bool_constant<detail::is_unwrappable_t<is_invocable, Func, Args...>::value>;
  };
  template <class Func, class Arg>
  constexpr auto is_applicable_v = is_applicable<Func, Arg>::value;

  // Calculates whether a given function could throw an exception when called with the
  // given argument, or unwrapped sequence of arguments if the argument is a tuple,
  // filtering out any instances of none.
  // Returns false if the function isn't invocable as specified.
  template <class Func, class Arg>
  struct is_nothrow_applicable : is_nothrow_invocable<Func, Arg> {
    using typename is_nothrow_invocable<Func, Arg>::canonical_type;
  };
  template <class Func, class... Args>
  struct is_nothrow_applicable<Func, std::tuple<Args...>> :
    detail::is_unwrappable_t<is_nothrow_invocable, Func, Args...>
  {
    using canonical_type =
      std::bool_constant<detail::is_unwrappable_t<is_nothrow_invocable, Func, Args...>::value>;
  };
  template <class Func, class Arg>
  constexpr auto is_nothrow_applicable_v = is_nothrow_applicable<Func, Arg>::value;

  // Calculates whether a given function object returns a type implicitly convertible to
  // the given return type when called with the provided argument, or unwrapped sequence
  // of arguments if the argument is a tuple, filtering out any instances of none.
  // Returns false if the function isn't invocable as specified.
  template <class Ret, class Func, class Arg>
  struct is_applicable_r : is_invocable_r<Ret, Func, Arg> {
    using typename is_invocable_r<Ret, Func, Arg>::canonical_type;
  };
  template <class Ret, class Func, class... Args>
  struct is_applicable_r<Ret, Func, std::tuple<Args...>> :
    detail::is_unwrappable_r_t<is_invocable_r, Ret, Func, Args...>
  {
    using canonical_type =
      std::bool_constant<detail::is_unwrappable_r_t<is_invocable_r, Ret, Func, Args...>::value>;
  };
  template <class Ret, class Func, class Arg>
  constexpr auto is_applicable_r_v = is_applicable_r<Ret, Func, Arg>::value;

  template <class Ret, class Func, class Arg>
  struct is_nothrow_applicable_r : is_nothrow_invocable_r<Ret, Func, Arg> {
    using typename is_nothrow_invocable_r<Ret, Func, Arg>::canonical_type;
  };
  template <class Ret, class Func, class... Args>
  struct is_nothrow_applicable_r<Ret, Func, std::tuple<Args...>> :
    detail::is_unwrappable_r_t<is_nothrow_invocable_r, Ret, Func, Args...>
  {
    using canonical_type =
      std::bool_constant<detail::is_unwrappable_r_t<is_nothrow_invocable_r, Ret, Func, Args...>::value>;
  };
  template <class Ret, class Func, class Arg>
  constexpr auto is_nothrow_applicable_r_v = is_nothrow_applicable_r<Ret, Func, Arg>::value;

  // Invoke implementation.
  namespace detail {
    template <class Functor, class Tuple, size_t... idxs>
    decltype(auto) invoke_impl(Functor&& func, Tuple&& args, std::index_sequence<idxs...>) {
      // A lambda to perform our invocation
      // so we don't have to write this bad boy multiple times.
      auto invocation = [&] () -> decltype(auto) {
        return std::invoke(std::forward<Functor>(func), std::get<idxs>(std::move(args))...);
      };

      // If our invocation will produce some non-void value, return that value.
      // Otherwise rewrite the return into daggr::meta::none.
      if constexpr (!std::is_void_v<decltype(invocation())>) {
        return invocation();
      } else {
        return invocation(), none {};
      }
    }
  }

  // Function takes any kind of callable, and applies the given sequence of arguments,
  // filtering out any instance of none.
  template <class Functor, class... Args, class =
    std::enable_if_t<
      meta::is_invocable_v<Functor, Args...>
    >
  >
  decltype(auto) invoke(Functor&& func, Args&&... the_args) {
    using idxs = filter_none_idxs_t<Args...>;
    return detail::invoke_impl(std::forward<Functor>(func),
        std::forward_as_tuple(std::forward<Args>(the_args)...), idxs {});
  }

  namespace detail {
    template <class Functor, class Tuple, size_t... idxs>
    decltype(auto) apply_impl(Functor&& func, Tuple&& args, std::index_sequence<idxs...>) {
      return meta::invoke(std::forward<Functor>(func), std::get<idxs>(std::forward<Tuple>(args))...);
    }
  }

  template <class Functor, class Arg, class =
    std::enable_if_t<
      meta::is_applicable_v<Functor, Arg>
    >
  >
  decltype(auto) apply(Functor&& func, Arg&& argument) {
    // If our functor is directly invocable with the passed argument, tuple or not...
    if constexpr (meta::is_invocable_v<Functor, Arg>) {
      // Invoke it directly.
      return meta::invoke(std::forward<Functor>(func), std::forward<Arg>(argument));
    } else {
      // Otherwise it must be a tuple we may or may not need to unpack.
      static_assert(meta::is_tuple_v<std::decay_t<Arg>>);
      static constexpr auto len = std::tuple_size<std::decay_t<Arg>>::value;

      // Our invocation of apply_impl here will indirect through meta::invoke,
      // filtering out any instances of meta::none.
      return detail::apply_impl([&] (auto&&... args) -> decltype(auto) {
        constexpr auto invoke_with_filtered_tuple =
            meta::is_invocable_v<Functor, std::tuple<std::decay_t<decltype(args)>...>>;

        // If our functor is directly invocable with the tuple post-filtering,
        if constexpr (invoke_with_filtered_tuple) {
          // Do so.
          return meta::invoke(std::forward<Functor>(func),
              std::tuple {std::forward<decltype(args)>(args)...});
        } else {
          // Otherwise invoke it with the arguments themselves.
          return meta::invoke(std::forward<Functor>(func), std::forward<decltype(args)>(args)...);
        }
      }, std::forward<Arg>(argument), std::make_index_sequence<len> {});
    }
  }

  // Function calculates the return type of any sequence of arguments applied to any functor.
  // Invocation must be known to be well formed prior to instantiation.
  template <class Functor, class... Args>
  struct invoke_result {
    using type = decltype(meta::invoke(std::declval<Functor>(), std::declval<Args>()...));
  };
  template <class Functor, class... Args>
  using invoke_result_t = typename invoke_result<Functor, Args...>::type;

  template <class Functor, class Arg>
  struct apply_result {
    using type = decltype(meta::apply(std::declval<Functor>(), std::declval<Arg>()));
  };
  template <class Functor, class Arg>
  using apply_result_t = typename apply_result<Functor, Arg>::type;

}

#endif
