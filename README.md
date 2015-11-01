# A library for easily querying mixpanel.

Mixpanel is great but sometimes you want to do some more advanced analysis. This library is meant to make exporting data from mixpanel easy. 


## Querying

The library provides two central function:

```haskell
mixpanel :: (MixpanelQuery a, FromJSON b) => MixpanelConfig -> a -> IO (Maybe b)
```

and 


```haskell
export :: (MixpanelQuery a, FromJSON b) => MixpanelConfig -> a -> IO [Maybe b]
```

The only differece between these two functions is `mixpanel` can be used to query all of mixpanels query endpoints other than events export, while `export` handles exporing events. The reason for this division is the event export retrurns afile with new line separated json objects instead of a valid json object. That difference can be seen in the signatures of both functions, where `mixpanel` returns a single json value, `export` returns a list of them.

### MixpanelConfig

To get a `MixpanelConfig` use the provided `defaultConfig` function to future proof your application.

```haskell

config = defaultConfig { apiKey = "API_KEY"
                       , apiSecret = "API_SECRET"
                       }
```

You will need to provide the `API_KEY` and `API_SECRET` from your own project.

### MixpanelQuery typeclass

The `MixpanelQuery` typeclass exists mostly for future expansion. Right now the is a single provided instance `instance MixpanelQuery (Map Text Text)`. This instance allows enough flexibilty to query all of mixpanels endpoints.

```haskell
surveyQuery   = insert "endpoint" "http://mixpanel.com/api/2.0/engage/"
              $ insert "selector" "any(properties[\"$answers\"], item[\"$survey_id\"] == 25000 and item[\"$collection_id\"] == 26000)"
              $ insert "limit" "1000"
              $ empty :: Map Text Text

``` 

As you can see we provide the full path to the endpoint we wish to query. The rest of the parameters depend on the endpoint you are qerying. The documented endpoints can be found [here][mixpanelapi].

[mixpanelapi]: https://mixpanel.com/docs/api-documentation/data-export-api

### Results

Results can be returned as any type with a FromJSON instance valid to the result from mixpanel. If you would like to use your own type feel free. If you don't have one you can use the default aeson json type:

```haskell
response <- mixpanel config query :: IO (Maybe Value)
```


